{-# LANGUAGE TemplateHaskell #-}

module Embed where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

lifxLogo :: ByteString
lifxLogo = $(embedFile "logo.png")

iconRefresh :: ByteString
iconRefresh = $(embedFile "noun-refresh-89040.png")
iconPower :: ByteString
iconPower = $(embedFile "noun-power-89049.png")
iconNext :: ByteString
iconNext = $(embedFile "noun-right-89087.png")
