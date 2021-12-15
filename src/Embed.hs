{-# LANGUAGE TemplateHaskell #-}

module Embed where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

lifxLogo :: ByteString
lifxLogo = $(embedFile "logo.png")
