
module BNFC.Backend.SV.CFtoInterp (cf2Interp) where

import Data.Char(toLower)

import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Common.OOAbstract
import BNFC.Backend.SV.Naming
import BNFC.Backend.SV.Utils

--Produces (.H file, .C file)
cf2Interp :: Maybe String -> CF -> (String, String)
cf2Interp inPackage cf = (mkHFile inPackage cab, mkCFile inPackage cab)
 where
    cab = cf2cabs cf

-- **** Header (.H) File Functions ****

--Generates the Header File
mkHFile :: Maybe String -> CAbs -> String
mkHFile inPackage cf = unlines [
  "`ifndef " ++ hdef,
  "`define " ++ hdef,
  "`include \"Absyn.svh\"",
  nsStart inPackage,
  "class Interp implements Visitor;",
  unlines ["  extern virtual task visit" ++ b ++ "(" ++ b ++ " p);" |
                              b <- classes, notElem b (defineds cf), notElem b (map fst (listtypes cf))],
  unlines ["  extern virtual task visit" ++ b ++ "(" ++ b ++ " " ++ map toLower b ++ ");" | b <- map fst (listtypes cf)],
  unlines ["  extern virtual task visit" ++ b ++ "(" ++ b ++  " x);" | b <- basics],
  unlines ["  task interpret(Visitable v);", "    v.accept(this);", "  endtask"],
  "endclass",
  nsEnd inPackage,
  "",
  "`endif"
 ]
 where
   hdef = nsDefine inPackage "INTERP_HEADER"
   classes = allClasses cf
   basics = tokentypes cf ++ map fst svbasetypes


-- **** Implementation (.svh) File Functions ****

--Makes the .svh File
mkCFile :: Maybe String -> CAbs -> String
mkCFile inPackage cf = unlines [
  headerC,
  nsStart inPackage,
  unlines [ "task Interp::visit" ++ t ++ "(" ++ t ++ " p); endtask //abstract class" | t <- absclasses cf],
  unlines [prCon   r  | (_,rs)  <- signatures cf, r <- rs],
  unlines [prList  cb | cb <- listtypes cf],
  unlines [prBasic b  | b  <- tokentypes cf ++ map fst svbasetypes],
  nsEnd inPackage
 ]

headerC :: String
headerC = unlines [
      "/*******************************************************/",
      "/* This implements the common visitor design pattern.",
      "   Note that this method uses Visitor-traversal of lists, so",
      "   List->accept() does NOT traverse the list. This allows different",
      "   algorithms to use context information differently. */",
      "",
      "`include \"Interp.svh\""
      ]

prBasic :: [Char] -> String
prBasic c = unlines [
  "task Interp::visit" ++ c ++ "(" ++ c ++ " x);",
  "  /* Code for " ++ c ++ " Goes Here */",
  "endtask"
  ]

prList :: ([Char], Bool) -> String
prList (cl,b) = unlines [
  "task Interp::visit" ++ cl ++ "("++ cl +++ " " ++ vname ++ ");",
  "  for ( int i = 0; i < "++ vname ++".v.size() ; i++)",
  "  begin",
  if b
    then "    " ++ vname ++ ".v[i].accept(this);"
    else "    visit" ++ drop 4 cl ++ "(" ++ vname ++ ".v[i]) ;",
  "  end",
  "endtask"
  ]
 where
   vname = map toLower cl

prCon :: (String, [([Char], Bool, [Char])]) -> String
prCon (f,cs) = unlines [
  "task Interp::visit" ++ f ++ "(" ++ f ++ " p);",
  "  /* Code For " ++ f ++ " Goes Here */",
  "",
  unlines ["  " ++ visitArg c | c <- cs],
  "endtask"
 ]
 where
   v = mkVariable f
   visitArg (cat,isPt,var) =
     if isPt
       then ("p." ++ var ++ ".accept(this);")
       else ("visit" ++ cat ++ "(p." ++ var ++ ");")
