{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import Network.Socket (PortNumber)
import Options.Generic (ParseField)

deriving anyclass instance ParseField PortNumber
