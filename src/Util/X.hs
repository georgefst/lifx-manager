module Util.X where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding
import Data.Traversable
import Foreign.C
import Graphics.X11
import Graphics.X11.Xlib.Extras

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

getIcon :: Window -> IO [CLong]
getIcon w = do
    d <- openDisplay ""
    netWmIcon <- internAtom d "_NET_WM_ICON" True
    Just x <- getWindowProperty32 d netWmIcon w
    pure x

setIcon :: Window -> [CLong] -> IO ()
setIcon w x = do
    d <- openDisplay ""
    netWmIcon <- internAtom d "_NET_WM_ICON" True
    changeProperty32 d w netWmIcon cARDINAL propModeReplace x
    flush d
