module Main (main) where

import Codec.BMP hiding (Error)
import Codec.Picture
import Control.Concurrent
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Colour.Names qualified as Colour
import Data.Colour.RGBSpace
import Data.Colour.SRGB (toSRGB, toSRGB24)
import Data.Foldable
import Data.Function
import Data.List.Extra
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Zipper qualified as Z
import Data.Maybe
import Data.Monoid.Extra
import Data.Optics.Operators
import Data.Text qualified as T
import Data.Traversable
import Data.Tuple.Extra hiding (first)
import Data.Vector.Storable qualified as V
import Data.Void
import Data.Word
import Embed
import Foreign (Storable)
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.SDL.Surface
import Lifx.Internal.Colour (hsbkToRgb)
import Lifx.Internal.ProductInfoMap qualified
import Lifx.Lan
import Lifx.Lan qualified as Lifx
import Lifx.Lan.Internal (LifxT (LifxT))
import Network.Socket
import OS.Window qualified as Window
import Optics hiding (both)
import Optics.State.Operators
import Options.Generic hiding (Product, unwrap)
import SDL.Font qualified as Font
import System.Exit
import System.Process (readProcess)
import Text.Pretty.Simple hiding (Color)
import Util.Gloss

data Opts = Opts
    { width :: Float <!> "0.5"
    -- ^ 0 to 1
    , height :: Float <!> "0.5"
    -- ^ 0 to 1
    , inc :: Word16 <!> "100"
    -- ^ for keyboard controls, reciprocal of fraction of full range
    , columns :: Int <!> "1000"
    , lineWidthProportion :: Float <!> "80"
    -- ^ divide the smaller of window width and height by this to get line width
    , devices :: Maybe Int
    -- ^ how many devices to look for at startup - if not given we just wait until timeout
    , timeout :: Int <!> "5"
    -- ^ how long to wait for message responses, in seconds
    , ip :: [IpV4]
    -- ^ hardcode some devices, instead of waiting for discovery
    , fake :: Bool
    -- ^ don't scan for devices at all - useful for testing/previewing with no network or bulbs
    }
    deriving (Generic, Show)
instance ParseRecord Opts where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

data ColourDimension
    = H
    | S
    | B
    | K
    deriving (Eq, Show, Generic, Enum, Bounded)
cdLens :: ColourDimension -> Lens' HSBK Word16
cdLens = \case
    H -> #hue
    S -> #saturation
    B -> #brightness
    K -> #kelvin
cdFromChar :: Char -> Maybe ColourDimension
cdFromChar = \case
    'h' -> Just H
    's' -> Just S
    'b' -> Just B
    'k' -> Just K
    _ -> Nothing
cdKeyDown :: Key -> Maybe ColourDimension
cdKeyDown = \case
    SpecialKey KeyLeft -> Just H
    Char '-' -> Just S
    SpecialKey KeyDown -> Just B
    Char '[' -> Just K
    _ -> Nothing
cdKeyUp :: Key -> Maybe ColourDimension
cdKeyUp = \case
    SpecialKey KeyRight -> Just H
    Char '=' -> Just S
    SpecialKey KeyUp -> Just B
    Char ']' -> Just K
    _ -> Nothing

data AppState = AppState
    { hsbk :: HSBK
    , power :: Bool
    , dimension :: Maybe ColourDimension
    -- ^ Which axis, if any, we are currently moving.
    , devices :: Z.Zipper Device'
    -- ^ All devices. Head is the active device.
    , windowWidth :: Float
    , windowHeight :: Float
    , lastError :: Maybe Error
    }
    deriving (Generic)
data Device' = Device' -- a device plus useful metadata
    { lifxDevice :: Device
    , deviceName :: Text
    , cdSupported :: ColourDimension -> Bool
    , cdLower :: ColourDimension -> Word16
    , cdUpper :: ColourDimension -> Word16
    }
    deriving (Generic)
data Error
    = LifxError LifxError
    | UnresponsiveDevice
    | OutOfRangeX Float
    | OutOfRangeY Float
    | BadDeviceIndex Word8
    | RescanFailed
    | RescanCurrentDeviceNotFound
    deriving (Show)

makeDevice' :: LightState -> Device -> Lifx.Product -> Device'
makeDevice' lightState lifxDevice prod =
    Device'
        { lifxDevice
        , deviceName = lightState.label
        , cdSupported = \case
            H -> prod.features.color
            S -> prod.features.color
            B -> True
            K -> kelvinLower /= kelvinUpper
        , cdLower = \case
            K -> kelvinLower
            _ -> minBound
        , cdUpper = \case
            K -> kelvinUpper
            _ -> maxBound
        }
  where
    (kelvinLower, kelvinUpper) =
        fromMaybe
            (minKelvin, maxKelvin)
            prod.features.temperatureRange

-- TODO we'd ideally use gloss-juicy here, but that library is unfortunately unmaintained and slightly rubbish:
-- https://github.com/alpmestan/gloss-juicy/issues/12

-- | Load a bitmap. Assume the input is monochrome, and output a colour based solely on the alpha value.
loadBsBmp :: RGB Word8 -> ByteString -> BitmapData
loadBsBmp c =
    bitmapDataOfBMP
        . unwrap
        . parseBMP
        . encodeBitmap @PixelRGBA8
        . ( \Image{..} ->
                Image
                    { imageData = mapVector4 (\_r _g _b a -> uncurryRGB (\r g b -> (r, g, b, a)) c) imageData
                    , ..
                    }
          )
        . unwrap
        . getRGBA8
        . unwrap
        . decodePng
  where
    -- like `fromRight undefined`, but shows a useful error
    unwrap = either (error . show) id
    getRGBA8 = \case
        ImageRGBA8 x -> pure x
        _ -> Left ()

bmpRefresh :: BitmapData
bmpRefresh = loadBsBmp (toSRGB24 (Colour.black :: Colour Double)) iconRefresh
bmpPower, bmpPowerWhite :: BitmapData
bmpPower = loadBsBmp (toSRGB24 (Colour.black :: Colour Double)) iconPower
bmpPowerWhite = loadBsBmp (toSRGB24 (Colour.white :: Colour Double)) iconPower

windowName :: Text
windowName = "LIFX"

main :: IO ()
main = do
    (opts :: Opts) <- getRecord "LIFX"
    (screenWidth, screenHeight) <- both fromIntegral <$> getScreenSize
    let windowWidth = screenWidth * unDefValue opts.width
        windowHeight = screenHeight * unDefValue opts.height
        lifxTimeout = unDefValue opts.timeout * 1_000_000
    fontFile <- readProcess "fc-match" ["-f%{file}"] []
    font <- Font.initialize >> Font.load fontFile 32
    devs <-
        if opts.fake
            then
                pure $
                    pure
                        ( LightState
                            { hsbk = HSBK 50000 25000 12500 6250
                            , label = "Fake device"
                            , power = 1
                            }
                        , deviceFromAddress (0, 0, 0, 0)
                        , either (error . ("Fake device: " <>) . show) id $
                            Lifx.Internal.ProductInfoMap.productLookup 1 1 0 0
                        )
            else
                let discover =
                        (<> map (deviceFromAddress . hostAddressToTuple . (.unwrap)) opts.ip)
                            <$> discoverDevices (subtract (length opts.ip) <$> opts.devices)
                 in maybe (liftIO $ putStrLn "timed out without finding any devices!" >> exitFailure) pure . nonEmpty
                        =<< either (lifxFailure "LIFX failure during discovery") pure
                        =<< runLifxT
                            lifxTimeout
                            (getExtraLightInfo =<< discover)
    let LightState{hsbk, power} = fst3 $ NE.head devs
    putStrLn "Found devices:"
    pPrintIndented $ NE.toList devs
    window <- newEmptyMVar -- MVar wrapper is due to the fact we can't get this before initialising Gloss
    let s0 =
            AppState
                { dimension = Nothing
                , devices = Z.fromNonEmpty $ uncurry3 makeDevice' <$> devs
                , lastError = Nothing
                , power = power /= 0
                , ..
                }
    absurd
        <$> interactM
            ( either (lifxFailure "LIFX initialisation failed") pure
                <=< runLifxT lifxTimeout
                    . LifxT
                    . flip evalStateT s0
            )
            ( InWindow
                (T.unpack windowName)
                ( round windowWidth
                , round windowHeight
                )
                ( round $ (screenWidth - windowWidth) / 2
                , round $ (screenHeight - windowHeight) / 2
                )
            )
            white
            (render font (unDefValue opts.lineWidthProportion) (unDefValue opts.columns) . snd)
            (coerce update (unDefValue opts.inc))
            ( either
                ( \case
                    RecvTimeout -> #lastError ?= UnresponsiveDevice
                    e -> do
                        pPrint e
                        #lastError ?= LifxError e
                )
                pure
            )
            ( const do
                w <- Window.findByName windowName
                Window.setIcon w lifxLogo
                putMVar window w
            )
  where
    lifxFailure t err = putStrLn (t <> ": " <> show err) >> exitFailure

render :: Font.Font -> Float -> Int -> AppState -> IO Picture
render font lineWidthProportion (fromIntegral -> columns) AppState{windowWidth = w, windowHeight = h, ..} = do
    deviceTexts <- fmap toList $ for devices $ fmap snd . bitmapOfSurface Cache <=< Font.blended font 0 . (.deviceName)
    let normalRow d =
            let l = fromIntegral $ dev.cdLower d
                u = fromIntegral $ dev.cdUpper d
             in pictures
                    [ -- background
                      pictures $
                        [0 .. columns - 1] <&> \x ->
                            let x' = (x + 0.5) / columns -- x coordinate of the bar's centre, in the interval [0,1]
                             in rectangleSolid columnWidth rectHeight
                                    & color (rgbToGloss . hsbkToRgb $ hsbk & cdLens d .~ round (x' * (u - l) + l))
                                    & translate (w * (x' - 0.5)) 0
                    , -- current value marker
                      translate (w * (fromIntegral (view (cdLens d) hsbk) - l) / (u - l)) 0
                        . translate (-w / 2) 0
                        $ if dimension == Just d
                            then
                                pictures
                                    [ rectangleSolid (lineWidth * 3) rectHeight
                                    , rectangleSolid lineWidth (rectHeight - lineWidth * 3)
                                        & color (rgbToGloss $ hsbkToRgb hsbk)
                                    ]
                            else rectangleSolid lineWidth rectHeight
                    ]
        bottomRow =
            pictures
                $ zipWith
                    ( \n ->
                        translate ((n / 2 + 0.5) * w' - w / 2) 0
                            . fromMaybe (rectangleSolid lineWidth rectHeight & color (rgbToGloss $ toSRGB Colour.black))
                    )
                    [0 ..]
                $ intersperse Nothing
                $ map Just contents
          where
            contents =
                [ if power
                    then drawBitmap bmpPower
                    else
                        pictures
                            [ rectangleSolid w' rectHeight & color (rgbToGloss $ toSRGB Colour.black)
                            , drawBitmap bmpPowerWhite
                            ]
                , drawBitmap bmpRefresh
                , pictures $
                    zipWith
                        ( \n p ->
                            translate 0 (rectHeight / 2 - (n + 0.5) * h') $
                                pictures $
                                    mwhen
                                        (n == genericLength (Z.lefts devices))
                                        [ rectangleSolid w' h' & color black
                                        , rectangleSolid (w' - lineWidth * 2) (h' - lineWidth * 2) & color white
                                        ]
                                        <> [p]
                        )
                        [0 ..]
                        deviceTexts
                ]
              where
                h' = rectHeight / genericLength deviceTexts
            w' = w / genericLength contents
            drawBitmap bmp =
                bitmap bmp
                    & join
                        scale
                        (uncurry min (bimap (w' /) (rectHeight /) . both fromIntegral $ bitmapSize bmp))
        dev = Z.current devices
        cdRows = map normalRow (filter dev.cdSupported enumerate) <> [bottomRow]
        rows = fromIntegral $ length cdRows
        ys = [rows / 2, rows / 2 - 1 .. -rows / 2]
        lineWidth = min w h / lineWidthProportion
        rectHeight = h / rows
        columnWidth = w / columns
    pure . pictures $
        zipWith
            (translate 0 . (rectHeight *) . subtract 0.5)
            ys
            cdRows
            <> map (\y -> translate 0 (y * rectHeight) $ rectangleSolid w lineWidth) ys
            <> maybe [] (pure . color red . scale 0.2 0.2 . text . show) lastError

update :: Word16 -> Event -> StateT AppState Lifx ()
update inc event = do
    w <- use #windowWidth
    h <- use #windowHeight
    devices <- use #devices
    let dev'@Device'{lifxDevice = dev, cdLower, cdUpper, cdSupported} = Z.current devices
        transform = bimap (f . (/ w)) (f . (/ h))
          where
            f = clamp (0, 1) . (+ 0.5)
        cdInc d = (cdUpper d - cdLower d) `div` inc
    case event of
        EventKey (MouseButton LeftButton) Down _ (transform -> (x, y)) ->
            if
                | y > 5 / rows -> #lastError ?= OutOfRangeY y
                | y > 4 / rows -> setColour H
                | y > 3 / rows -> setColour S
                | y > 2 / rows -> setColour B
                | y > 1 / rows -> setColour K
                | y >= 0 ->
                    -- TODO synchronise this with rendering
                    let bottomRowCols = 3
                     in if
                            | x > 3 / bottomRowCols -> #lastError ?= OutOfRangeX x
                            | x > 2 / bottomRowCols ->
                                let n = floor $ (1 - y * rows) * fromIntegral (length devices)
                                 in -- TODO we should really use a data structure with constant-time indexing
                                    case applyN n Z.right (Z.start devices) of
                                        Just ds' -> setDevices ds'
                                        Nothing -> #lastError ?= BadDeviceIndex n
                            | x > 1 / bottomRowCols -> rescan
                            | x > 0 / bottomRowCols -> togglePower dev
                            | otherwise -> #lastError ?= OutOfRangeX x
                | otherwise -> #lastError ?= OutOfRangeY y
          where
            rows = genericLength (filter cdSupported enumerate) + 1
            setColour d = do
                #dimension ?= d
                setColourFromX dev' x
        EventKey (MouseButton LeftButton) Up _ (transform -> (x, _y)) -> do
            setColourFromX dev' x
            #dimension .= Nothing
        EventKey (MouseButton RightButton) Up _ _ ->
            togglePower dev
        EventKey (SpecialKey KeySpace) Down _ _ ->
            togglePower dev
        EventKey (Char (cdFromChar -> Just d)) Down _ _ ->
            #dimension ?= d
        EventKey (cdKeyDown -> Just d) Down _ _ -> do
            #hsbk % cdLens d -= cdInc d
            updateColour dev
        EventKey (cdKeyUp -> Just d) Down _ _ -> do
            #hsbk % cdLens d += cdInc d
            updateColour dev
        EventKey (SpecialKey KeyEsc) Down _ _ ->
            #dimension .= Nothing
        EventMotion (transform -> (x, _y)) ->
            setColourFromX dev' x
        EventKey (Char 'l') Down _ _ ->
            setDevices $ fromMaybe (Z.start devices) $ Z.right devices
        EventKey (Char 'r') Down _ _ ->
            rescan
        EventKey (Char 'f') Down _ _ ->
            refreshState dev
        EventResize (w', h') -> do
            #windowWidth .= fromIntegral w'
            #windowHeight .= fromIntegral h'
        _ -> pure ()
  where
    setDevices ds@(Z.current -> Device'{..}) = do
        success <-
            (refreshState lifxDevice >> pure True)
                `catchError` \e -> throwError e >> pure False
        when success $ #devices .= ds
    updateColour dev = sendMessage dev . flip SetColor 0 =<< use #hsbk
    refreshState dev = do
        #lastError .= Nothing
        LightState{hsbk, power} <- sendMessage dev GetColor
        #hsbk .= hsbk
        #power .= (power /= 0)
    rescan =
        (fmap (fmap Z.fromNonEmpty . nonEmpty) . getExtraLightInfo =<< discoverDevices Nothing) >>= \case
            Nothing -> #lastError ?= RescanFailed
            Just ds -> do
                #lastError .= Nothing
                old <- (.deviceName) . Z.current <$> use #devices
                dsz <-
                    maybe
                        (#lastError ?= RescanCurrentDeviceNotFound >> pure ds)
                        pure
                        (findRightZ ((== old) . (.label) . fst3) ds)
                let LightState{hsbk, power} = fst3 $ Z.current dsz
                #hsbk .= hsbk
                #power .= (power /= 0)
                #devices .= (uncurry3 makeDevice' <$> dsz)
    togglePower dev = do
        p <- not <$> use #power
        #power .= p
        sendMessage dev $ SetPower p
    setColourFromX dev x =
        use #dimension >>= \case
            Nothing -> pure ()
            Just d -> do
                #hsbk % cdLens d .= round (u * x - l * (x - 1))
                updateColour dev.lifxDevice
              where
                l = fromIntegral $ dev.cdLower d
                u = fromIntegral $ dev.cdUpper d

getExtraLightInfo :: (MonadLifx m) => [Device] -> m [(LightState, Device, Lifx.Product)]
getExtraLightInfo =
    traverse
        ( \dev ->
            (,dev,)
                <$> sendMessage dev GetColor
                <*> getProductInfo dev
        )

{- Util -}

clamp :: (Ord a) => (a, a) -> a -> a
clamp (l, u) = max l . min u

rgbToGloss :: RGB Float -> Color
rgbToGloss RGB{..} =
    makeColor
        channelRed
        channelGreen
        channelBlue
        1

-- min, max across all devices
minKelvin :: (Num a) => a
minKelvin = 1500
maxKelvin :: (Num a) => a
maxKelvin = 9000

pPrintIndented :: (MonadIO m, Show a) => a -> m ()
pPrintIndented = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4}

mapVector4 :: (Storable a) => (a -> a -> a -> a -> (a, a, a, a)) -> V.Vector a -> V.Vector a
mapVector4 f v = flip (V.unfoldrExactN $ V.length v) (0, []) $ uncurry \n -> \case
    [] ->
        let l i = v V.! i
            (r0, r1, r2, r3) = f (l n) (l $ n + 1) (l $ n + 2) (l $ n + 3)
         in (r0, (n + 4, [r1, r2, r3]))
    x : xs -> (x, (n, xs))

-- TODO this belongs in a library
newtype IpV4 = IpV4 {unwrap :: HostAddress}
    deriving stock (Generic)
    deriving anyclass (ParseRecord, ParseField, ParseFields)
instance Show IpV4 where
    show (IpV4 x) = intercalate "." $ map show [a, b, c, d]
      where
        (a, b, c, d) = hostAddressToTuple x
instance Read IpV4 where
    readsPrec _ s = case map read $ splitOn "." s of
        [a, b, c, d] -> pure $ (,"") $ IpV4 $ tupleToHostAddress (a, b, c, d)
        _ -> []

applyN :: (Integral n, Monad f) => n -> (b -> f b) -> b -> f b
applyN 0 _ = pure
applyN n g = g <=< applyN (n - 1) g

-- TODO upstream (first version, which is very similar to `findRight`, but we can't use as constructor isn't exported)
findRightZ :: (a -> Bool) -> Z.Zipper a -> Maybe (Z.Zipper a)
-- findRightZ target z@(Z.Zipper ps curr ns)
--     | target curr = Just z
--     | otherwise = case ns of
--         [] -> Nothing
--         (x : xs) -> findRightZ target (Z.Zipper (curr : ps) x xs)
findRightZ target z
    | target (Z.current z) = Just z
    | otherwise = findRightZ target =<< Z.right z
