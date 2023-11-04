module Embed.Util where

import Data.FileEmbed (embedFile)
import Language.Haskell.TH (Exp, Q)
import System.FilePath ((</>))

-- | Needs to be in its own module due to the phase restriction.
embed :: FilePath -> Q Exp
embed = embedFile . ("embed" </>)
