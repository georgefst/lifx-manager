{-# LANGUAGE TemplateHaskell #-}

module Embed where

import Data.ByteString (ByteString)
import Embed.Util (embed)

lifxLogo :: ByteString
lifxLogo = $(embed "logo.png")
iconRefresh :: ByteString
iconRefresh = $(embed "noun-refresh-89040.png")
iconPower :: ByteString
iconPower = $(embed "noun-power-89049.png")
