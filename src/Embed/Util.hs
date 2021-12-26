module Embed.Util where

import Data.FileEmbed (embedFile)
import Language.Haskell.TH (Exp, Q)

embed :: FilePath -> Q Exp
embed = embedFile
