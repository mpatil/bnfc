
module BNFC.Backend.SV.CFtoInterp (cf2Interp) where

import Data.Char(toLower)

import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Common.OOAbstract
import BNFC.Backend.SV.Config
import BNFC.Backend.SV.Utils

-- Produces generated base header/impl and write-once user header/impl.
cf2Interp :: Maybe String -> String -> CF -> (String, String, String, String)
cf2Interp inPackage name cf = (mkBaseHFile inPackage cab name, mkBaseCFile inPackage cab name, mkUserHFile inPackage cab name, mkUserCFile inPackage cab name)
 where
    cab = cf2cabs cf

-- **** Base Header (.svh) File Functions ****

mkBaseHFile :: Maybe String -> CAbs -> String -> String
mkBaseHFile inPackage cf name = unlines [
  "`ifndef " ++ svUpperName cfg ++ "_"  ++ hdef,
  "`define " ++ svUpperName cfg ++ "_"  ++ hdef,
  "`include \"" ++ svAbsynHeaderPath cfg ++ "\"",
  nsStart inPackage,
  "virtual class InterpBase implements Visitor;",
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
   cfg = mkSVConfig inPackage name
   hdef = nsDefine inPackage "INTERP_BASE_HEADER"
   classes = allClasses cf
   basics = tokentypes cf ++ map fst svbasetypes

-- **** User Header (.svh) File Functions ****

mkUserHFile :: Maybe String -> CAbs -> String -> String
mkUserHFile inPackage cf name = unlines
  [ "`ifndef " ++ svUpperName cfg ++ "_" ++ hdef
  , "`define " ++ svUpperName cfg ++ "_" ++ hdef
  , "`include \"" ++ svInterpBaseHeaderPath cfg ++ "\""
  , ""
  , "/* User interpreter declarations."
  , "   This file is generated once and then preserved."
  , "   Add semantic state, helper classes, and method declarations here. */"
  , nsStart inPackage
  , "class Interp extends InterpBase;"
  , unlines ["  extern virtual task visit" ++ b ++ "(" ++ b ++ " p);" |
                              b <- classes, notElem b (defineds cf), notElem b (map fst (listtypes cf))]
  , unlines ["  extern virtual task visit" ++ b ++ "(" ++ b ++ " " ++ map toLower b ++ ");" | b <- map fst (listtypes cf)]
  , unlines ["  extern virtual task visit" ++ b ++ "(" ++ b ++  " x);" | b <- basics]
  , "endclass"
  , nsEnd inPackage
  , ""
  , "`endif"
  ]
  where
    cfg = mkSVConfig inPackage name
    hdef = nsDefine inPackage "INTERP_HEADER"
    classes = allClasses cf
    basics = tokentypes cf ++ map fst svbasetypes

-- **** Base Implementation (.sv) File Functions ****

mkBaseCFile :: Maybe String -> CAbs -> String -> String
mkBaseCFile inPackage cf name = unlines [
  baseHeaderC cfg,
  nsStart inPackage,
  unlines [ "task InterpBase::visit" ++ t ++ "(" ++ t ++ " p); endtask //abstract class" | t <- absclasses cf],
  unlines [prCon   r  | (_,rs)  <- signatures cf, r <- rs],
  unlines [prList  cb | cb <- listtypes cf],
  unlines [prBasic b  | b  <- tokentypes cf ++ map fst svbasetypes],
  nsEnd inPackage
 ]
 where
  cfg = mkSVConfig inPackage name

baseHeaderC :: SVConfig -> String
baseHeaderC cfg = unlines [
      "/*******************************************************/",
      "/* This implements the common visitor design pattern.",
      "   Note that this method uses Visitor-traversal of lists, so",
      "   List->accept() does NOT traverse the list. This allows different",
      "   algorithms to use context information differently. */",
      "",
      "`include \"" ++ svInterpBaseHeaderPath cfg ++ "\""
      ]

-- **** User Implementation (.sv) File Functions ****

mkUserCFile :: Maybe String -> CAbs -> String -> String
mkUserCFile inPackage cf name = unlines
  [ userHeaderC cfg
  , nsStart inPackage
  , unlines [ "task Interp::visit" ++ t ++ "(" ++ t ++ " p); super.visit" ++ t ++ "(p); endtask" | t <- absclasses cf]
  , unlines [prUserCon   r  | (_,rs)  <- signatures cf, r <- rs]
  , unlines [prUserList  cb | cb <- listtypes cf]
  , unlines [prUserBasic b  | b  <- tokentypes cf ++ map fst svbasetypes]
  , nsEnd inPackage
  ]
 where
  cfg = mkSVConfig inPackage name

userHeaderC :: SVConfig -> String
userHeaderC cfg = unlines
  [ "/*******************************************************/"
  , "/* User interpreter extension points."
   , "   This file is generated once and then preserved."
  , "   Override the visit methods below with your semantics. */"
  , ""
  , "`include \"" ++ svInterpHeaderPath cfg ++ "\""
  ]

prBasic :: [Char] -> String
prBasic c = unlines [
  "task InterpBase::visit" ++ c ++ "(" ++ c ++ " x);",
  "  /* Code for " ++ c ++ " Goes Here */",
  "endtask"
  ]

prList :: ([Char], Bool) -> String
prList (cl,b) = unlines [
  "task InterpBase::visit" ++ cl ++ "("++ cl +++ " " ++ vname ++ ");",
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
  "task InterpBase::visit" ++ f ++ "(" ++ f ++ " p);",
  "  /* Code For " ++ f ++ " Goes Here */",
  "",
  unlines ["  " ++ visitArg c | c <- cs],
  "endtask"
 ]
 where
   visitArg (cat,isPt,var) =
     if isPt
       then ("p." ++ var ++ ".accept(this);")
       else ("visit" ++ cat ++ "(p." ++ var ++ ");")

prUserBasic :: [Char] -> String
prUserBasic c = unlines
  [ "task Interp::visit" ++ c ++ "(" ++ c ++ " x);"
  , "  super.visit" ++ c ++ "(x);"
  , "endtask"
  ]

prUserList :: ([Char], Bool) -> String
prUserList (cl,_) = unlines
  [ "task Interp::visit" ++ cl ++ "(" ++ cl +++ " " ++ map toLower cl ++ ");"
  , "  super.visit" ++ cl ++ "(" ++ map toLower cl ++ ");"
  , "endtask"
  ]

prUserCon :: (String, [([Char], Bool, [Char])]) -> String
prUserCon (f,_) = unlines
  [ "task Interp::visit" ++ f ++ "(" ++ f ++ " p);"
  , "  super.visit" ++ f ++ "(p);"
  , "endtask"
  ]
