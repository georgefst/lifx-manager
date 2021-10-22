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
import Data.Tuple.Extra hiding (first)
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
    , columns :: Int <!> "1000"
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

data AppState = AppState
    { hsbk :: HSBK
    , power :: Bool
    , -- | Which axis, if any, we are currently moving.
      dimension :: Maybe ColourDimension
    , -- | All devices. Head is the active device.
      devices :: Stream (Text, Device)
    , windowWidth :: Float
    , windowHeight :: Float
    , lastError :: Maybe Error
    }
    deriving (Show, Generic)
data Error
    = LifxError LifxError
    | OutOfRangeX Float
    | OutOfRangeY Float
    deriving (Show)

main :: IO ()
main = do
    Opts{..} <- getRecord "LIFX"
    (screenWidth, screenHeight) <- both fromIntegral <$> getScreenSize
    let windowWidth = screenWidth * unDefValue width
        windowHeight = screenHeight * unDefValue height
    devs <-
        maybe (liftIO $ putStrLn "timed out without finding any devices!" >> exitFailure) pure . nonEmpty
            =<< runLifx (discoverDevices devices >>= traverse (\dev -> (,dev) <$> sendMessage dev GetColor))
    let LightState{hsbk, power} = fst $ NE.head devs
    putStrLn "Found devices:"
    pPrintIndented $ NE.toList devs
    let s0 =
            AppState
                { dimension = Nothing
                , devices = Stream.cycle $ first (decodeUtf8 . view #label) <$> devs
                , lastError = Nothing
                , power = power /= 0
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
                        #lastError .= Just (LifxError e)
                    )
                    pure
                )
                mempty

render :: Float -> Int -> AppState -> (Picture, String)
render lineWidthProportion (fromIntegral -> columns) AppState{windowWidth = w, windowHeight = h, ..} =
    (,title) . pictures $
        zipWith
            ( \md y -> translate 0 ((y - 0.5) * rectHeight) case md of
                Just d ->
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
                -- the bottom row - there's only one 'Nothing' in the list
                Nothing ->
                    pictures
                        [ -- power
                          pictures
                            [ rectangleSolid w' rectHeight
                                & color (if power then white else black)
                            , circleSolid (min w' rectHeight / 5)
                                & color (if power then black else white)
                            ]
                            & translate (- w' / 2) 0
                        , -- next device
                          let scale' = map (both (/ 50) . ((* w) *** (* h)))
                              rectPoints =
                                scale'
                                    [ (1, -1)
                                    , (-3, -1)
                                    , (-3, 1)
                                    , (1, 1)
                                    ]
                              trianglePoints =
                                scale'
                                    [ (1, 1)
                                    , (1, 2)
                                    , (3, 0)
                                    , (1, -2)
                                    , (1, -1)
                                    ]
                           in pictures
                                [ rectangleSolid w' rectHeight
                                    & color (rgbToGloss $ hsbkToRgb hsbk)
                                , pictures
                                    [ polygon rectPoints
                                    , polygon trianglePoints
                                    ]
                                    & color (rgbToGloss $ invertRGB $ hsbkToRgb hsbk)
                                , line $ rectPoints <> trianglePoints
                                ]
                                & translate (w' / 2) 0
                        , rectangleSolid lineWidth rectHeight
                        ]
                  where
                    w' = w / 2
            )
            (map Just enumerate <> [Nothing])
            ys
            <> map (\y -> translate 0 (y * rectHeight) $ rectangleSolid w lineWidth) ys
            <> maybe [] (pure . color red . scale 0.2 0.2 . text . show) lastError
  where
    rows = 5
    ys = [rows / 2, rows / 2 - 1 .. - rows / 2]
    lineWidth = min w h / lineWidthProportion
    rectHeight = h / rows
    columnWidth = w / columns
    title =
        unwords
            [ T.unpack (fst . streamHead $ devices)
            , "-"
            , "LIFX"
            ]

update :: Word16 -> Event -> StateT AppState Lifx ()
update inc event = do
    w <- use #windowWidth
    h <- use #windowHeight
    let transform = bimap (f . (/ w)) (f . (/ h))
          where
            f = clamp (0, 1) . (+ 0.5)
    dev <- snd . streamHead <$> use #devices
    case event of
        EventKey (MouseButton LeftButton) Down _ (transform -> (x, y)) ->
            --TODO Fourmolu should do better here - hang the `if` or at least avoid double indenting
            if
                    | y > 1.00 -> #lastError .= Just (OutOfRangeY y)
                    | y > 0.80 -> setColour H
                    | y > 0.60 -> setColour S
                    | y > 0.40 -> setColour B
                    | y > 0.20 -> setColour K
                    | y >= 0.00 ->
                        if
                                | x > 1.0 -> #lastError .= Just (OutOfRangeX x)
                                | x > 0.5 -> nextDevice
                                | x > 0.0 -> togglePower dev
                                | otherwise -> #lastError .= Just (OutOfRangeX x)
                    | otherwise -> #lastError .= Just (OutOfRangeY y)
          where
            setColour d = do
                #dimension .= Just d
                setColourFromX dev x
        EventKey (MouseButton LeftButton) Up _ (transform -> (x, _y)) -> do
            setColourFromX dev x
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
            setColourFromX dev x
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
        (name, dev') <- streamHead <$> use #devices
        liftIO . T.putStrLn $ "Switching device: " <> name
        refreshState dev'
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
    setColourFromX dev x =
        use #dimension >>= \case
            Nothing -> pure ()
            Just d -> do
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

invertRGB :: RGB Float -> RGB Float
invertRGB RGB{..} =
    RGB
        { channelRed = 1 - channelRed
        , channelGreen = 1 - channelGreen
        , channelBlue = 1 - channelBlue
        }

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

pPrintIndented :: (MonadIO m, Show a) => a -> m ()
pPrintIndented = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4}

--TODO upstream: https://github.com/well-typed/optics/issues/433
(+=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
l += x = l %= (+ x)
(-=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
l -= x = l %= subtract x
