module Main (main) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.Coerce
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV (hsv)
import Data.List.Extra
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Stream.Infinite (Stream)
import Data.Stream.Infinite qualified as Stream
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as T
import Data.Tuple.Extra
import Data.Word
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact
import Lifx.Lan
import Optics hiding (both)
import Optics.State.Operators
import Options.Generic
import System.Exit
import Text.Pretty.Simple hiding (Color)
import Util.Gloss

data Opts = Opts
    { -- | 0 to 1
      width :: Float <!> "0.5"
    , -- | 0 to 1
      height :: Float <!> "0.5"
    , -- | for keyboard controls, reciprocal of fraction of full range
      inc :: Word16 <!> "100"
    , columns :: Int <!> "100"
    , -- | divide the smaller of window width and height by this to get line width
      lineWidthProportion :: Float <!> "80"
    , -- | how many devices to look for at startup - if not given we just wait until default timeout
      devices :: Maybe Int
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
cdLower :: ColourDimension -> Word16
cdLower = \case
    K -> 1500
    _ -> minBound
cdUpper :: ColourDimension -> Word16
cdUpper = \case
    K -> 9000
    _ -> maxBound
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
cdFromY :: Float -> Maybe ColourDimension
cdFromY y
    | y > 1.00 = Nothing
    | y > 0.75 = Just H
    | y > 0.50 = Just S
    | y > 0.25 = Just B
    | y > 0.00 = Just K
    | otherwise = Nothing

data AppState = AppState
    { hsbk :: HSBK
    , power :: Bool
    , -- | Which axis, if any, we are currently moving.
      dimension :: Maybe ColourDimension
    , -- | All devices. Head is the active device.
      devices :: Stream (Text, Device)
    , windowWidth :: Float
    , windowHeight :: Float
    , lastError :: Maybe LifxError
    }
    deriving (Show, Generic)

main :: IO ()
main = do
    Opts{..} <- getRecord "LIFX"
    (screenWidth, screenHeight) <- both fromIntegral <$> getScreenSize
    let windowWidth = screenWidth * unDefValue width
        windowHeight = screenHeight * unDefValue height
    (devs, hsbk, power) <- runLifx do
        (nonEmpty <$> discoverDevices' devices) >>= \case
            Just devs -> do
                LightState{..} <- sendMessage (snd $ NE.head devs) GetColor
                pure (devs, hsbk, power /= 0)
            Nothing -> liftIO $ putStrLn "timed out without finding any devices!" >> exitFailure
    putStrLn "Found devices:"
    pPrintIndented devs
    let s0 =
            AppState
                { dimension = Nothing
                , devices = Stream.cycle devs
                , lastError = Nothing
                , ..
                }
    runLifx . LifxT $
        flip evalStateT s0 $
            interactM
                ( InWindow
                    "LIFX"
                    ( round windowWidth
                    , round windowHeight
                    )
                    ( round $ (screenWidth - windowWidth) / 2
                    , round $ (screenHeight - windowHeight) / 2
                    )
                )
                white
                ( pure
                    . render (unDefValue lineWidthProportion) (unDefValue columns)
                    . snd
                )
                (coerce update (unDefValue inc))
                ( either
                    ( \e -> do
                        pPrint e
                        #lastError .= Just e
                    )
                    pure
                )
                mempty

render :: Float -> Int -> AppState -> (Picture, String)
render lineWidthProportion (fromIntegral -> columns) AppState{windowWidth = w, windowHeight = h, ..} =
    (,title) . pictures $
        zipWith
            ( \d y ->
                let l = fromIntegral $ cdLower d
                    u = fromIntegral $ cdUpper d
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
                            . translate (- w / 2) 0
                            $ if dimension == Just d
                                then
                                    pictures
                                        [ rectangleSolid (lineWidth * 3) rectHeight
                                        , rectangleSolid lineWidth (rectHeight - lineWidth * 3)
                                            & color (rgbToGloss $ hsbkToRgb hsbk)
                                        ]
                                else rectangleSolid lineWidth rectHeight
                        ]
                        & translate 0 (rectHeight * y)
            )
            enumerate
            [3 / 2, 1 / 2 ..]
            <> map
                (flip (translate 0) $ rectangleSolid w lineWidth)
                (take 5 [rectHeight * 2, rectHeight ..])
            <> maybe [] (pure . color red . scale 0.2 0.2 . text . show) lastError
  where
    lineWidth = min w h / lineWidthProportion
    rectHeight = h / 4
    columnWidth = w / columns
    title =
        unwords $
            [ "LIFX"
            , "-"
            , T.unpack (fst . streamHead $ devices)
            ]
                <> mwhen (not power) ["(powered off)"]

update :: Word16 -> Event -> StateT AppState Lifx ()
update inc event = do
    w <- use #windowWidth
    h <- use #windowHeight
    let transform = bimap (f . (/ w)) (f . (/ h))
          where
            f = clamp (0, 1) . (+ 0.5)
    dev <- snd . streamHead <$> use #devices
    case event of
        EventKey (MouseButton LeftButton) Down _ (transform -> (x, y)) -> do
            #dimension .= cdFromY y
            maybe (pure ()) (setColourFromX dev x) $ cdFromY y
        EventKey (MouseButton LeftButton) Up _ (transform -> (x, _y)) -> do
            maybe (pure ()) (setColourFromX dev x) =<< use #dimension
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
            use #dimension >>= maybe (pure ()) (setColourFromX dev x)
        EventKey (Char 'l') Down _ _ -> do
            #devices %= Stream.tail
            (name, dev') <- streamHead <$> use #devices
            liftIO . T.putStrLn $ "Switching device: " <> name
            refreshState dev'
        EventKey (Char 'r') Down _ _ ->
            refreshState dev
        EventResize (w', h') -> do
            #windowWidth .= fromIntegral w'
            #windowHeight .= fromIntegral h'
        _ -> pure ()
  where
    updateColour dev = sendMessage dev . flip SetColor 0 =<< use #hsbk
    refreshState dev = do
        #lastError .= Nothing
        LightState{hsbk, power} <- sendMessage dev GetColor
        #hsbk .= hsbk
        #power .= (power /= 0)
    togglePower dev = do
        p <- not <$> use #power
        #power .= p
        sendMessage dev $ SetPower p
    cdInc d = (cdUpper d - cdLower d) `div` inc
    setColourFromX dev x d = do
        #hsbk % cdLens d .= round (u * x - l * (x - 1))
        updateColour dev
      where
        l = fromIntegral $ cdLower d
        u = fromIntegral $ cdUpper d

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

hsbkToRgb :: HSBK -> RGB Float
hsbkToRgb HSBK{..} =
    interpolateColour
        (fromIntegral saturation / maxWord16)
        c
        c'
  where
    -- no Kelvin
    c =
        hsv
            (360 * fromIntegral hue / maxWord16)
            (fromIntegral saturation / maxWord16)
            (fromIntegral brightness / maxWord16)
    -- just Kelvin
    c' =
        let t =
                (log (fromIntegral kelvin) - log (fromIntegral $ cdLower K))
                    / log (fromIntegral (cdUpper K) / fromIntegral (cdLower K))
         in clamp (0, 1)
                <$> RGB
                    { channelRed = 1
                    , channelGreen = t / 2 + 0.5
                    , channelBlue = t
                    }

interpolateColour :: Num a => a -> RGB a -> RGB a -> RGB a
interpolateColour r = liftA2 (\a b -> a * (r + b * (1 - r)))

maxWord16 :: Float
maxWord16 = fromIntegral $ maxBound @Word16

--TODO for some reason, there is no Stream.head: https://github.com/ekmett/streams/pull/19
streamHead :: Stream a -> a
streamHead = (Stream.!! 0)

discoverDevices' :: MonadLifx m => Maybe Int -> m [(Text, Device)]
discoverDevices' nDevices =
    discoverDevices nDevices
        >>= traverse
            (\dev -> (,dev) . decodeUtf8 . view #label <$> sendMessage dev GetColor)

pPrintIndented :: (MonadIO m, Show a) => a -> m ()
pPrintIndented = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4}

mwhen :: Monoid p => Bool -> p -> p
mwhen b x = if b then x else mempty

--TODO upstream: https://github.com/well-typed/optics/issues/433
(+=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
l += x = l %= (+ x)
(-=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
l -= x = l %= subtract x
