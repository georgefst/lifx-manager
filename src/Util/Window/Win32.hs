module Util.Window.Win32 (
    Window, -- it's important that the implementation is hidden here, since it will vary between platforms
    findByName,
    setTitle,
    setIcon,
) where

import Codec.Picture
import Control.Monad
import Data.ByteString (ByteString, useAsCString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Storable qualified as Vec
import Foreign.Ptr
import Graphics.Win32
import Unsafe.Coerce

newtype Window = Window HWND

findByName ::
    -- | substring which must appear in the window title
    Text ->
    IO Window
findByName name = do
    Just w <- findWindow Nothing . Just $ T.unpack name
    pure $ Window w

setTitle :: Window -> Text -> IO ()
setTitle (Window w) t = setWindowText w $ T.unpack t

setIcon ::
    Window ->
    -- | PNG image
    ByteString ->
    IO ()
setIcon (Window w) img = case decodePng img of
    Left e -> error e
    Right (ImageRGBA8 Image{..}) -> do
        let bs = BS.pack . reorderPixels $ Vec.toList imageData
        icon <- useAsCString bs $ createIcon nullPtr imageWidth imageHeight 1 32 nullPtr . castPtr
        void $ sendMessage w wM_SETICON iCON_BIG $ unsafeCoerce icon
      where
        reorderPixels = \case
            r : g : b : a : ps ->
                b : g : r : a : reorderPixels ps
            [] -> []
            _ -> error "vector length not a multiple of 4"
    _ -> error "wrong pixel type"

--TODO these should all be upstreamed to Win32 - see https://github.com/haskell/win32/pull/194
{- HLINT ignore "Use camelCase" -}
createIcon :: HINSTANCE -> Int -> Int -> BYTE -> BYTE -> Ptr BYTE -> Ptr BYTE -> IO HICON
createIcon instance_ width height planes bitsPixel andBits xorBits =
    failIfNull "CreateIcon" $ c_CreateIcon instance_ width height planes bitsPixel andBits xorBits
foreign import ccall unsafe "windows.h CreateIcon" -- when upstreaming, use WINDOWS_CCONV rather than ccall
    c_CreateIcon :: HINSTANCE -> Int -> Int -> BYTE -> BYTE -> Ptr BYTE -> Ptr BYTE -> IO HICON
wM_SETICON :: WindowMessage -- https://github.com/haskell/win32/blob/master/Graphics/Win32/Message.hsc
wM_SETICON = 128
iCON_BIG :: WPARAM -- not sure there's currently a relevant module for this (also, add `iCON_SMALL = 0`)
iCON_BIG = 1
