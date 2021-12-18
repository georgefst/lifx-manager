module Util.Window.X11 (
    Window, -- it's important that the implementation is hidden here, since it will vary between platforms
    findByName,
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
import Unsafe.Coerce

newtype Window = Window X11.Window

findByName ::
    -- | substring which must appear in the window title
    Text ->
    IO Window
findByName name = do
    Just (w, _) <- do
        d <- openDisplay ""
        netClientList <- internAtom d "_NET_CLIENT_LIST" True
        Just ids <- getWindowProperty32 d netClientList (defaultRootWindow d)
        find ((name `T.isInfixOf`) . snd) <$> for ids \(fromIntegral -> i) -> do
            Just cs <- getWindowProperty8 d wM_NAME i
            pure (i, decodeLatin1 . BS.pack $ map fromIntegral cs)
    pure $ Window w

setIcon ::
    Window ->
    -- | PNG image
    ByteString ->
    IO ()
setIcon (Window w) img = do
    case decodePng img of
        Left e -> error e
        Right (ImageRGBA8 Image{..}) -> do
            d <- openDisplay ""
            netWmIcon <- internAtom d "_NET_WM_ICON" True
            changeProperty32 d w netWmIcon cARDINAL propModeReplace $
                map fromIntegral [imageWidth, imageHeight]
                    ++ map unsafeCoerce (groupPixels $ Vec.toList imageData)
            flush d
          where
            groupPixels :: [Word8] -> [Word64]
            groupPixels = \case
                r : g : b : a : ps ->
                    ( shift (unsafeCoerce a) 24
                        .|. shift (unsafeCoerce r) 16
                        .|. shift (unsafeCoerce g) 8
                        .|. shift (unsafeCoerce b) 0
                    ) :
                    groupPixels ps
                [] -> []
                _ -> error "vector length not a multiple of 4"
        _ -> error "wrong pixel type"
