module Util.Window.X11 (
    Window, -- it's important that the implementation is hidden here, since it will vary between platforms
    findByName,
    setTitle,
    setIcon,
) where

import Codec.Picture
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Traversable
import Data.Vector.Storable qualified as Vec
import Data.Word
import Graphics.X11 hiding (Window)
import Graphics.X11 qualified as X11
import Graphics.X11.Xlib.Extras

data Window = Window X11.Window Display
    deriving (Eq, Ord)

findByName ::
    -- | substring which must appear in the window title
    Text ->
    IO Window
findByName name = do
    d <- openDisplay ""
    Just (w, _) <- do
        nET_CLIENT_LIST <- internAtom d "_NET_CLIENT_LIST" True
        Just ids <- getWindowProperty32 d nET_CLIENT_LIST (defaultRootWindow d)
        find ((name `T.isInfixOf`) . snd) <$> for ids \(fromIntegral -> i) -> do
            Just cs <- getWindowProperty8 d wM_NAME i
            pure (i, decodeLatin1 . BS.pack $ map fromIntegral cs)
    pure $ Window w d

setTitle :: Window -> Text -> IO ()
setTitle (Window w d) t = do
    nET_WM_NAME <- internAtom d "_NET_WM_NAME" True
    uTF8_STRING <- internAtom d "UTF8_STRING" True
    changeProperty8 d w nET_WM_NAME uTF8_STRING propModeReplace . map fromIntegral . BS.unpack $ encodeUtf8 t
    flush d

setIcon ::
    Window ->
    -- | PNG image
    ByteString ->
    IO ()
setIcon (Window w d) img = do
    case decodePng img of
        Left e -> error e
        Right (ImageRGBA8 Image{..}) -> do
            nET_WM_ICON <- internAtom d "_NET_WM_ICON" True
            changeProperty32 d w nET_WM_ICON cARDINAL propModeReplace $
                map fromIntegral [imageWidth, imageHeight]
                    ++ map fromIntegral (groupPixels $ Vec.toList imageData)
            flush d
          where
            groupPixels :: [Word8] -> [Word64]
            groupPixels = \case
                r : g : b : a : ps ->
                    ( shift (fromIntegral a) 24
                        .|. shift (fromIntegral r) 16
                        .|. shift (fromIntegral g) 8
                        .|. shift (fromIntegral b) 0
                    ) :
                    groupPixels ps
                [] -> []
                _ -> error "vector length not a multiple of 4"
        _ -> error "wrong pixel type"
