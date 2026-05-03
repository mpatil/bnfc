module BNFC.Backend.SV.PrettyPrinter (cf2SVPrinter, prRender) where

import BNFC.CF
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.SV.PrettyPrinter.Header
import BNFC.Backend.SV.PrettyPrinter.Impl
import BNFC.Backend.SV.PrettyPrinter.Render

--Produces (.H file, .C file)
cf2SVPrinter :: Bool -> Maybe String -> String -> CF -> (String, String)
cf2SVPrinter _ inPackage name cf =
    (mkHFile inPackage name cf groups, mkCFile inPackage name cf groups)
 where
    groups = positionRules cf ++ fixCoercions (ruleGroupsInternals cf)

positionRules :: CF -> [(Cat,[Rule])]
positionRules cf =
  [ (TokenCat cat, [ Rule (noPosition cat) (noPosition $ TokenCat cat) (map (Left . TokenCat) [catString, catInteger]) Parsable ])
  | cat <- filter (isPositionCat cf) $ map fst (tokenPragmas cf)
  ]
