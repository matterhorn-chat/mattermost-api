module Network.Mattermost.TH where

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH

import Lens.Micro ((&), (.~))
import Lens.Micro.TH (DefName(..), makeLensesWith, lensRules, lensField)

suffixLenses :: TH.Name -> TH.DecsQ
suffixLenses = makeLensesWith $
  lensRules & lensField .~ (\_ _ name -> [TopName $ TH.mkName $ TH.nameBase name ++ "L"])
