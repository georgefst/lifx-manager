module Util.Window.Unsupported (
    Window, -- it's important that the implementation is hidden here, since it will vary between platforms
    findByName,
    setTitle,
    setIcon,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import System.IO

newtype Window = Window ()
    deriving (Eq, Ord)

findByName ::
    -- | substring which must appear in the window title
    Text ->
    IO Window
findByName _name = do
    warn
    pure $ Window ()

setTitle :: Window -> Text -> IO ()
setTitle (Window ()) _t =
    warn

setIcon ::
    Window ->
    -- | PNG image
    ByteString ->
    IO ()
setIcon (Window ()) _img = do
    warn

warn :: IO ()
warn = hPutStrLn stderr "Warning: window utilities unsupported on this OS"
