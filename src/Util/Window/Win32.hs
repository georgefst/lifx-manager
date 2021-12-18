module Util.Window.Win32 (setWindowIcon) where

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

setWindowIcon ::
    -- | substring which must appear in the window title
    Text ->
    -- | PNG image
    ByteString ->
    IO ()
setWindowIcon name img = do
    Just hWnd <- findWindow Nothing . Just $ T.unpack name
    case decodePng img of
        Left e -> error e
        Right (ImageRGBA8 Image{..}) -> do
            let bs = BS.pack . reorderPixels $ Vec.toList imageData
            icon <- useAsCString bs $ createIcon nullPtr imageWidth imageHeight 1 32 nullPtr . castPtr
            void $ sendMessage hWnd wM_SETICON iCON_BIG $ unsafeCoerce icon
          where
            reorderPixels = \case
                r : g : b : a : ps ->
                    b : g : r : a : reorderPixels ps
                [] -> []
                _ -> error "vector length not a multiple of 4"
        _ -> error "wrong pixel type"

--TODO these should all be upstreamed to Win32
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
