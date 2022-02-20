module Main (main) where

import Codec.BMP hiding (Error)
import Codec.Picture
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Colour.RGBSpace
import Data.Composition
import Data.List.Extra
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Stream.Infinite (Stream)
import Data.Stream.Infinite qualified as Stream
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple.Extra hiding (first)
import Data.Word
import Embed
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact
import Lifx.Internal.Colour (hsbkToRgb)
import Lifx.Internal.ProductInfoMap qualified
import Lifx.Lan
import Lifx.Lan.Internal (LifxT (LifxT)) -- TODO avoid this somehow
import Optics hiding (both)
import Optics.State.Operators
import Options.Generic hiding (Product, unwrap)
import System.Exit
import System.Timeout
import Text.Pretty.Simple hiding (Color)
import Util.Gloss
import Util.Window qualified as Window

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
    -- ^ how many devices to look for at startup - if not given we just wait until default timeout
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
    , devices :: Stream Device'
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
    | OutOfRangeX Float
    | OutOfRangeY Float
    deriving (Show)

-- TODO we'd ideally use gloss-juicy here, but that library is unfortunately unmaintained and slightly rubbish:
-- https://github.com/alpmestan/gloss-juicy/issues/12
loadBsBmp :: ByteString -> BitmapData
loadBsBmp = bitmapDataOfBMP . unwrap . parseBMP . unwrap . encodeDynamicBitmap . unwrap . decodePng
  where
    -- like `fromRight undefined`, but shows a useful error
    unwrap = either (error . show) id
bmpRefresh :: BitmapData
bmpRefresh = loadBsBmp iconRefresh
bmpPower :: BitmapData
bmpPower = loadBsBmp iconPower
bmpNext :: BitmapData
bmpNext = loadBsBmp iconNext

{- | The value of this doesn't really matter since it gets overwritten near-instantly at startup.
But, since we use `Window.findByName`, we should try to make sure other windows are unlikely to share it.
On some OSs, this may also remain the window "name" (semantic), while we only change the "title" (visual).
-}
initialWindowName :: Text
initialWindowName = "Haskell LIFX Manager"

setWindowTitle :: AppState -> Window.Window -> IO ()
setWindowTitle AppState{..} w =
    Window.setTitle w . T.unwords $
        mconcat
            [
                [ (streamHead devices).deviceName
                ]
            , mwhen
                (not power)
                [ "(powered off)"
                ]
            ,
                [ "-"
                , "LIFX"
                ]
            ]

main :: IO ()
main = do
    Opts{..} <- getRecord "LIFX"
    (screenWidth, screenHeight) <- both fromIntegral <$> getScreenSize
    let windowWidth = screenWidth * unDefValue width
        windowHeight = screenHeight * unDefValue height
    devs <-
        if fake
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
                maybe (liftIO $ putStrLn "timed out without finding any devices!" >> exitFailure) pure . nonEmpty
                    =<< runLifx
                        ( discoverDevices devices
                            >>= traverse
                                ( \dev ->
                                    (,dev,)
                                        <$> sendMessage dev GetColor
                                        <*> getProductInfo dev
                                )
                        )
    let LightState{hsbk, power} = fst3 $ NE.head devs
    putStrLn "Found devices:"
    pPrintIndented $ NE.toList devs
    window <- newEmptyMVar -- MVar wrapper is due to the fact we can't get this before initialising Gloss
    let s0 =
            AppState
                { dimension = Nothing
                , devices =
                    Stream.cycle $
                        devs
                            <&> \(lightState, lifxDevice, prod) ->
                                let (kelvinLower, kelvinUpper) =
                                        fromMaybe (minKelvin, maxKelvin) $
                                            prod ^. #features % #temperatureRange
                                 in Device'
                                        { lifxDevice
                                        , deviceName = lightState ^. #label
                                        , cdSupported = \case
                                            H -> prod ^. #features % #color
                                            S -> prod ^. #features % #color
                                            B -> True
                                            K -> True
                                        , cdLower = \case
                                            K -> kelvinLower
                                            _ -> minBound
                                        , cdUpper = \case
                                            K -> kelvinUpper
                                            _ -> maxBound
                                        }
                , lastError = Nothing
                , power = power /= 0
                , ..
                }
    runLifx . LifxT
        . flip evalStateT s0
        $ interactM
            ( InWindow
                (T.unpack initialWindowName)
                ( round windowWidth
                , round windowHeight
                )
                ( round $ (screenWidth - windowWidth) / 2
                , round $ (screenHeight - windowHeight) / 2
                )
            )
            white
            (render (unDefValue lineWidthProportion) (unDefValue columns) . snd)
            (coerce update window (unDefValue inc))
            ( either
                ( \e -> do
                    pPrint e
                    #lastError .= Just (LifxError e)
                )
                pure
            )
            ( const do
                w <- Window.findByName initialWindowName
                Window.setIcon w lifxLogo
                setWindowTitle s0 w
                putMVar window w
            )

render :: Float -> Int -> AppState -> IO Picture
render lineWidthProportion (fromIntegral -> columns) AppState{windowWidth = w, windowHeight = h, ..} =
    pure . pictures $
        zipWith
            ( \md y -> translate 0 ((y - 0.5) * rectHeight) case md of
                Just d ->
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
                -- the bottom row - there's only one 'Nothing' in the list
                Nothing ->
                    pictures
                        [ drawBitmap bmpPower & translate (-w') 0
                        , drawBitmap bmpRefresh
                        , drawBitmap bmpNext & translate w' 0
                        , rectangleSolid lineWidth rectHeight
                            & translate (-w' / 2) 0
                        , rectangleSolid lineWidth rectHeight
                            & translate (w' / 2) 0
                        ]
                  where
                    w' = w / 3
                    drawBitmap bmp =
                        bitmap bmp
                            & join
                                scale
                                (uncurry min (bimap (w' /) (rectHeight /) . both fromIntegral $ bitmapSize bmp))
            )
            cdRows
            ys
            <> map (\y -> translate 0 (y * rectHeight) $ rectangleSolid w lineWidth) ys
            <> maybe [] (pure . color red . scale 0.2 0.2 . text . show) lastError
  where
    dev = streamHead devices
    cdRows = map Just (filter dev.cdSupported enumerate) <> [Nothing]
    rows = fromIntegral $ length cdRows
    ys = [rows / 2, rows / 2 - 1 .. -rows / 2]
    lineWidth = min w h / lineWidthProportion
    rectHeight = h / rows
    columnWidth = w / columns

update :: MVar Window.Window -> Word16 -> Event -> StateT AppState Lifx ()
update winMVar inc event = do
    w <- use #windowWidth
    h <- use #windowHeight
    dev'@Device'{lifxDevice = dev, cdLower, cdUpper, cdSupported} <- streamHead <$> use #devices
    let transform = bimap (f . (/ w)) (f . (/ h))
          where
            f = clamp (0, 1) . (+ 0.5)
        cdInc d = (cdUpper d - cdLower d) `div` inc
    case event of
        EventKey (MouseButton LeftButton) Down _ (transform -> (x, y)) ->
            -- TODO Fourmolu should do better here - hang the `if` or at least avoid double indenting
            if
                    | y > 5 / rows -> #lastError .= Just (OutOfRangeY y)
                    | y > 4 / rows -> setColour H
                    | y > 3 / rows -> setColour S
                    | y > 2 / rows -> setColour B
                    | y > 1 / rows -> setColour K
                    | y >= 0 ->
                        if
                                | x > 3 / bottomRowCols -> #lastError .= Just (OutOfRangeX x)
                                | x > 2 / bottomRowCols -> nextDevice
                                | x > 1 / bottomRowCols -> refreshState dev
                                | x > 0 / bottomRowCols -> togglePower dev
                                | otherwise -> #lastError .= Just (OutOfRangeX x)
                    | otherwise -> #lastError .= Just (OutOfRangeY y)
          where
            bottomRowCols = 3
            rows = genericLength (filter cdSupported enumerate) + 1
            setColour d = do
                #dimension .= Just d
                setColourFromX dev' x
        EventKey (MouseButton LeftButton) Up _ (transform -> (x, _y)) -> do
            setColourFromX dev' x
            #dimension .= Nothing
        EventKey (MouseButton RightButton) Up _ _ ->
            togglePower dev
        EventKey (SpecialKey KeySpace) Down _ _ ->
            togglePower dev
        EventKey (Char (cdFromChar -> Just d)) Down _ _ ->
            #dimension .= Just d
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
            nextDevice
        EventKey (Char 'r') Down _ _ ->
            refreshState dev
        EventResize (w', h') -> do
            #windowWidth .= fromIntegral w'
            #windowHeight .= fromIntegral h'
        _ -> pure ()
  where
    nextDevice = do
        #devices %= Stream.tail
        Device'{..} <- streamHead <$> use #devices
        s <- get
        liftIO do
            T.putStrLn $ "Switching device: " <> deviceName
            w <- maybe (putStrLn "Initialisation failure" >> exitFailure) pure =<< timeout 1_000_000 (readMVar winMVar)
            setWindowTitle s w
        refreshState lifxDevice
    updateColour dev = sendMessage dev . flip SetColor 0 =<< use #hsbk
    refreshState dev = do
        #lastError .= Nothing
        LightState{hsbk, power} <- sendMessage dev GetColor
        #hsbk .= hsbk
        #power .= (power /= 0)
        join $ gets (liftIO .: setWindowTitle) <*> liftIO (readMVar winMVar)
    togglePower dev = do
        p <- not <$> use #power
        #power .= p
        join $ gets (liftIO .: setWindowTitle) <*> liftIO (readMVar winMVar)
        sendMessage dev $ SetPower p
    setColourFromX dev x =
        use #dimension >>= \case
            Nothing -> pure ()
            Just d -> do
                #hsbk % cdLens d .= round (u * x - l * (x - 1))
                updateColour $ dev.lifxDevice
              where
                l = fromIntegral $ dev.cdLower d
                u = fromIntegral $ dev.cdUpper d

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
minKelvin :: Num a => a
minKelvin = 1500
maxKelvin :: Num a => a
maxKelvin = 9000

-- TODO for some reason, there is no Stream.head: https://github.com/ekmett/streams/pull/19
streamHead :: Stream a -> a
streamHead = (Stream.!! 0)

pPrintIndented :: (MonadIO m, Show a) => a -> m ()
pPrintIndented = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4}

-- TODO upstream: https://github.com/well-typed/optics/issues/433
(+=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
l += x = l %= (+ x)
(-=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
l -= x = l %= subtract x

-- TODO I've used this in a lot of projects by now - it should probably go in something like `extra`
mwhen :: Monoid p => Bool -> p -> p
mwhen b x = if b then x else mempty
