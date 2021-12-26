module Embed.Util where

import Data.FileEmbed (embedFile)
import Language.Haskell.TH (Exp, Q)
import System.FilePath ((</>))

embed :: FilePath -> Q Exp
embed = embedFile . ("embed" </>)
