module Main (main) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Composition
import Data.List.Extra
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
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
import Text.Pretty.Simple

data Opts = Opts
    { -- | local address of target light
      ip :: Ip
    , -- | 0 to 1
      width :: Float
    , -- | 0 to 1
      height :: Float
    }
    deriving (Generic, Show, ParseRecord)

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
        (pure . render . fst)
        (update windowWidth $ unIp ip)
        mempty
  where
    f a ((b, c), d) = (b, (c, (a, d)))

render :: AppState -> Picture
render s = translate (-220) 80 . scale 0.2 0.2 . text' 150 . TL.toStrict $ pShowNoColor s

update :: Float -> HostAddress -> Event -> StateT AppState Lifx ()
update windowWidth addr = \case
    EventKey (MouseButton LeftButton) Up _ _ -> sendMessage addr $ SetPower True
    EventKey (MouseButton RightButton) Up _ _ -> sendMessage addr $ SetPower False
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
            sendMessage addr . flip SetColor (Duration 0) =<< gets (view #hsbk)
    _ -> pure ()

{- Config -}

colour0 :: HSBK
colour0 = HSBK 0 0 30_000 2_500

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
