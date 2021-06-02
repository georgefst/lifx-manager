module Main (main) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Composition
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word
import GHC.Generics (Generic)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Lifx.Lan
import Network.Socket
import Optics
import Optics.State.Operators
import Text.Pretty.Simple

data AttrKey
    = H
    | S
    | B
    | K
    deriving (Show, Generic)
data AppState = AppState
    { hsbk :: HSBK
    , attrKey :: Maybe AttrKey
    }
    deriving (Show, Generic)

main :: IO ()
main = do
    (e, s0) <- runLifx $ ((,) .: (,)) <$> getSocket <*> getSource <*> getCounter
    interactM
        (\x (a, (_e, s)) -> f e <$> runReaderT (runStateT (unLifxT (runStateT x a)) s) e)
        (InWindow "LIFX" (windowWidth, windowHeight) (windowX0, windowY0))
        white
        (AppState colour0 Nothing, (e, s0))
        (pure . render . fst)
        update
        mempty
  where
    f a ((b, c), d) = (b, (c, (a, d)))

render :: AppState -> Picture
render s = translate (-220) 80 . scale 0.2 0.2 . text' 150 . TL.toStrict $ pShowNoColor s

update :: Event -> StateT AppState Lifx ()
update = \case
    EventKey (MouseButton LeftButton) Up _ _ -> sendMessage bedroomLightAddr $ SetPower True
    EventKey (MouseButton RightButton) Up _ _ -> sendMessage bedroomLightAddr $ SetPower False
    EventKey (Char 'h') Down _ _ -> #attrKey .= Just H
    EventKey (Char 's') Down _ _ -> #attrKey .= Just S
    EventKey (Char 'b') Down _ _ -> #attrKey .= Just B
    EventKey (Char 'k') Down _ _ -> #attrKey .= Just K
    EventKey (SpecialKey KeyEsc) Down _ _ -> #attrKey .= Nothing
    EventMotion (clamp (- windowWidth / 2, windowWidth / 2) -> x, _y) ->
        gets (view #attrKey) >>= \case
            Just H -> updateLight #hue 1
            Just S -> updateLight #saturation 1
            Just B -> updateLight #brightness 1
            Just K -> updateLight #kelvin 8
            _ -> pure ()
      where
        updateLight l m = do
            #hsbk % l .= round ((x + windowWidth / 2) * fromIntegral (maxBound @Word16) / windowWidth / m)
            sendMessage bedroomLightAddr . flip SetColor (Duration 0) =<< gets (view #hsbk)
    _ -> pure ()

{- Config -}

windowWidth :: Num a => a
windowWidth = 700
windowHeight :: Int
windowHeight = 400
windowX0 :: Int
windowX0 = 10
windowY0 :: Int
windowY0 = 10

colour0 :: HSBK
colour0 = HSBK 0 0 30_000 2_500

bedroomLightAddr :: HostAddress
bedroomLightAddr = tupleToHostAddress (192, 168, 1, 190)

{- Util -}

-- | Like 'text' but reflects newlines.
text' :: Float -> Text -> Picture
text' spacing =
    pictures
        . zipWith (\i -> translate 0 (i * (- spacing))) [0 ..]
        . map (text . T.unpack)
        . T.lines

clamp :: (Ord a) => (a, a) -> a -> a
clamp (l, u) = max l . min u

-- | Like 'interactIO', but with the update function in an arbitrary `StateT s IO`-like monad.
interactM ::
    (forall a. m a -> s -> IO (a, s)) ->
    Display ->
    Graphics.Gloss.Color ->
    s ->
    (s -> IO Picture) ->
    (Event -> m ()) ->
    (Controller -> IO ()) ->
    IO ()
interactM trans d c s v u = interactIO d c s v (fmap snd .: (trans . u))
