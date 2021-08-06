module Main (main) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
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

data Opts = Opts
    { -- | 0 to 1
      width :: Float <!> "0.5"
    , -- | 0 to 1
      height :: Float <!> "0.5"
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
    deriving (Show, Generic, Enum, Bounded)
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

data AppState = AppState
    { hsbk :: HSBK
    , power :: Bool
    , -- | Which axis, if any, we are currently moving.
      dimension :: Maybe ColourDimension
    , -- | All devices. Head is the active device.
      devices :: Stream (Text, Device)
    }
    deriving (Show, Generic)

main :: IO ()
main = do
    Opts{..} <- getRecord "LIFX"
    (screenWidth, screenHeight) <- both fromIntegral <$> getScreenSize
    let windowWidth = screenWidth * unDefValue width
        windowHeight = screenHeight * unDefValue height
    (devs, colour0, power0, e, s0) <- runLifx do
        (nonEmpty <$> discoverDevices' devices) >>= \case
            Just devs -> do
                LightState{hsbk = colour0, power = power0} <- sendMessage (snd $ NE.head devs) GetColor
                (devs,colour0,power0 /= 0,,) <$> ((,,) <$> getSocket <*> getSource <*> getTimeout) <*> getCounter
            Nothing -> liftIO $ putStrLn "timed out without finding any devices!" >> exitFailure
    putStrLn "Found devices:"
    pPrintIndented devs
    interactM
        ( \(a, (_e, s)) x ->
            runExceptT (runReaderT (runStateT (unLifxT (runStateT x a)) s) e) >>= \case
                Left err -> pPrint err >> pure Nothing
                Right r -> pure . pure $ f e r
        )
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
        (AppState colour0 power0 Nothing (Stream.cycle devs), (e, s0))
        ( pure
            . render (unDefValue lineWidthProportion) (unDefValue columns) (windowWidth, windowHeight)
            . fst
        )
        (update windowWidth)
        mempty
  where
    f a ((b, c), d) = (b, (c, (a, d)))

render :: Float -> Int -> (Float, Float) -> AppState -> (Picture, String)
render lineWidthProportion (fromIntegral -> columns) (w, h) AppState{..} =
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
                          rectangleSolid lineWidth rectHeight
                            & translate (- w / 2) 0
                            & translate (w * (fromIntegral (view (cdLens d) hsbk) - l) / (u - l)) 0
                        ]
                        & translate 0 (rectHeight * y)
            )
            enumerate
            [3 / 2, 1 / 2 ..]
            <> map
                (flip (translate 0) $ rectangleSolid w lineWidth)
                (take 5 [rectHeight * 2, rectHeight ..])
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

update :: Float -> Event -> StateT AppState Lifx ()
update w event = do
    addr <- snd . streamHead <$> use #devices
    case event of
        EventKey (MouseButton LeftButton) Up _ _ -> do
            #power .= True
            sendMessage addr . SetPower =<< use #power
        EventKey (MouseButton RightButton) Up _ _ -> do
            #power .= False
            sendMessage addr . SetPower =<< use #power
        EventKey (SpecialKey KeySpace) Down _ _ -> do
            p <- (== 0) . view #power <$> sendMessage addr GetPower
            #power .= p
            sendMessage addr $ SetPower p
        EventKey (Char (cdFromChar -> Just d)) Down _ _ -> #dimension .= Just d
        EventKey (SpecialKey KeyEsc) Down _ _ -> #dimension .= Nothing
        EventMotion (clamp (0, 1) . (+ 0.5) . (/ w) -> x, _y) ->
            use #dimension >>= maybe (pure ()) \d -> do
                let l = fromIntegral $ cdLower d
                    u = fromIntegral $ cdUpper d
                #hsbk % cdLens d .= round (u * x - l * (x - 1))
                sendMessage addr . flip SetColor 0 =<< use #hsbk
        EventKey (Char 'l') Down _ _ -> do
            #devices %= Stream.tail
            (name, addr') <- streamHead <$> use #devices
            liftIO . T.putStrLn $ "Switching device: " <> name
            refreshState addr'
        EventKey (Char 'r') Down _ _ ->
            refreshState addr
        _ -> pure ()
  where
    refreshState addr = do
        LightState{hsbk, power} <- sendMessage addr GetColor
        #hsbk .= hsbk
        #power .= (power /= 0)

{- Util -}

clamp :: (Ord a) => (a, a) -> a -> a
clamp (l, u) = max l . min u

-- | Like 'interactIO', but with the update function in an arbitrary `StateT s (MaybeT IO)`-like monad.
interactM ::
    (forall a. s -> m a -> IO (Maybe (a, s))) ->
    Display ->
    Color ->
    s ->
    (s -> IO (Picture, String)) ->
    (Event -> m ()) ->
    (Controller -> IO ()) ->
    IO ()
interactM trans d c s0 v u = interactIO d c s0 v (flip \s -> fmap (maybe s snd) . trans s . u)

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
            (\addr -> (,addr) . decodeUtf8 . view #label <$> sendMessage addr GetColor)

pPrintIndented :: (MonadIO m, Show a) => a -> m ()
pPrintIndented = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4}

mwhen :: Monoid p => Bool -> p -> p
mwhen b x = if b then x else mempty
