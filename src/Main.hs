module Main (main) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Composition
import Data.List.Extra
import Data.Tuple.Extra
import Data.Word
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact
import Lifx.Lan
import Network.Socket
import Optics hiding (both)
import Optics.State.Operators
import Options.Generic

data Opts = Opts
    { -- | local address of target light
      ip :: Ip
    , -- | 0 to 1
      width :: Float
    , -- | 0 to 1
      height :: Float
    , columns :: Int
    }
    deriving (Generic, Show, ParseRecord)

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

data AppState = AppState
    { hsbk :: HSBK
    , -- | Which axis, if any, we are currently moving.
      dimension :: Maybe ColourDimension
    }
    deriving (Show, Generic)

main :: IO ()
main = do
    Opts{..} <- getRecord "LIFX"
    (screenWidth, screenHeight) <- both fromIntegral <$> getScreenSize
    let windowWidth = screenWidth * width
        windowHeight = screenHeight * height
    (e, s0) <- runLifx $ ((,) .: (,)) <$> getSocket <*> getSource <*> getCounter
    interactM
        (\x (a, (_e, s)) -> f e <$> runReaderT (runStateT (unLifxT (runStateT x a)) s) e)
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
        (AppState colour0 Nothing, (e, s0))
        (pure . render columns (windowWidth, windowHeight) . fst)
        (update windowWidth $ unIp ip)
        mempty
  where
    f a ((b, c), d) = (b, (c, (a, d)))

render :: Int -> (Float, Float) -> AppState -> Picture
render (fromIntegral -> columns) (w, h) AppState{..} =
    pictures $
        zipWith
            ( \d y ->
                pictures
                    [ -- background
                      pictures $
                        [0 .. columns - 1] <&> \x ->
                            let x' = (x + 0.5) / columns -- x coordinate of the bar's centre, in the interval [0,1]
                             in rectangleSolid columnWidth rectHeight
                                    & color (rgbToGloss . hsbkToRgb $ hsbk & cdLens d .~ round (x' * maxWord16))
                                    & translate (w * (x' - 0.5)) 0
                    , -- current value marker
                      rectangleSolid lineWidth rectHeight
                        & translate (- w / 2) 0
                        & translate
                            ( w * fromIntegral (view (cdLens d) hsbk - cdLower d)
                                / fromIntegral (cdUpper d - cdLower d)
                            )
                            0
                    ]
                    & translate 0 (rectHeight * y)
            )
            enumerate
            [3 / 2, 1 / 2 ..]
            <> map
                (flip (translate 0) $ rectangleSolid w lineWidth)
                (take 5 [rectHeight * 2, rectHeight ..])
  where
    lineWidth = min w h * lineWidthFactor
    rectHeight = h / 4
    columnWidth = w / columns

update :: Float -> HostAddress -> Event -> StateT AppState Lifx ()
update w addr = \case
    EventKey (MouseButton LeftButton) Up _ _ -> sendMessage addr $ SetPower True
    EventKey (MouseButton RightButton) Up _ _ -> sendMessage addr $ SetPower False
    EventKey (Char 'h') Down _ _ -> #dimension .= Just H
    EventKey (Char 's') Down _ _ -> #dimension .= Just S
    EventKey (Char 'b') Down _ _ -> #dimension .= Just B
    EventKey (Char 'k') Down _ _ -> #dimension .= Just K
    EventKey (SpecialKey KeyEsc) Down _ _ -> #dimension .= Nothing
    EventMotion (clamp (0, 1) . (+ 0.5) . (/ w) -> x, _y) ->
        gets (view #dimension) >>= maybe (pure ()) \d -> do
            let l = fromIntegral $ cdLower d
                u = fromIntegral $ cdUpper d
            #hsbk % cdLens d .= round (u * x - l * (x - 1))
            sendMessage addr . flip SetColor (Duration 0) =<< gets (view #hsbk)
    _ -> pure ()

{- Config -}

colour0 :: HSBK
colour0 = HSBK 0 0 30_000 2_500

-- this is as a fraction of the smaller of window width and height
lineWidthFactor :: Float
lineWidthFactor = 1 / 80

{- Util -}

clamp :: (Ord a) => (a, a) -> a -> a
clamp (l, u) = max l . min u

-- | Like 'interactIO', but with the update function in an arbitrary `StateT s IO`-like monad.
interactM ::
    (forall a. m a -> s -> IO (a, s)) ->
    Display ->
    Color ->
    s ->
    (s -> IO Picture) ->
    (Event -> m ()) ->
    (Controller -> IO ()) ->
    IO ()
interactM trans d c s v u = interactIO d c s v (fmap snd .: (trans . u))

newtype Ip = Ip {unIp :: HostAddress}
    deriving stock (Generic)
    deriving anyclass (ParseRecord, ParseField, ParseFields)
instance Show Ip where
    show (Ip x) = intercalate "." $ map show [a, b, c, d]
      where
        (a, b, c, d) = hostAddressToTuple x
instance Read Ip where
    readsPrec _ s = case map read $ splitOn "." s of
        [a, b, c, d] -> pure . (,"") . Ip $ tupleToHostAddress (a, b, c, d)
        _ -> []

rgbToGloss :: RGB Float -> Color
rgbToGloss RGB{..} =
    makeColor
        channelRed
        channelGreen
        channelBlue
        1

-- | Note theat this ignores temperature.
hsbkToRgb :: HSBK -> RGB Float
hsbkToRgb HSBK{..} =
    hsv
        (360 * fromIntegral hue / maxWord16)
        (fromIntegral saturation / maxWord16)
        (fromIntegral brightness / maxWord16)

maxWord16 :: Float
maxWord16 = fromIntegral $ maxBound @Word16
