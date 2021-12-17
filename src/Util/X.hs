module Util.X (setWindowIcon) where

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
import Foreign.C
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Unsafe.Coerce

setWindowIcon ::
    -- | substring which must appear in the window title
    Text ->
    -- | PNG image
    ByteString ->
    IO ()
setWindowIcon name img = do
    Just (w, _) <- find ((name `T.isInfixOf`) . snd) <$> getWindows
    setIconJuicy w img

getWindows :: IO [(Window, Text)]
getWindows = do
    d <- openDisplay ""
    netClientList <- internAtom d "_NET_CLIENT_LIST" True
    Just ids <- getWindowProperty32 d netClientList (defaultRootWindow d)
    for ids \(fromIntegral -> i) -> (i,) <$> getName i

getName :: Window -> IO Text
getName w = do
    d <- openDisplay ""
    Just cs <- getWindowProperty8 d wM_NAME w
    pure . decodeUtf8 . BS.pack $ map fromIntegral cs

setIcon :: Window -> [CLong] -> IO ()
setIcon w x = do
    d <- openDisplay ""
    netWmIcon <- internAtom d "_NET_WM_ICON" True
    changeProperty32 d w netWmIcon cARDINAL propModeReplace x
    flush d

setIconJuicy :: Window -> ByteString -> IO ()
setIconJuicy w bs =
    case decodePng bs of
        Left e -> error e
        Right (ImageRGBA8 Image{..}) ->
            setIcon w $
                map fromIntegral [imageWidth, imageHeight]
                    ++ map unsafeCoerce (groupPixels $ Vec.toList imageData)
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
