{-# LANGUAGE TupleSections #-}

module BNFC.Backend.SV.CFtoAbs (cf2SVAbs) where

import Data.List
import Data.Char(toLower, isUpper, toUpper)

import BNFC.CF
import BNFC.Options (RecordPositions(..))
import BNFC.TypeChecker ( ListConstructors(..), buildContext, isToken )
import BNFC.Utils((+++))
import BNFC.Backend.Common.OOAbstract

import BNFC.Backend.SV.Naming
import BNFC.Backend.SV.Utils

--The result is two files (.h file, .sv file)

cf2SVAbs :: RecordPositions -> Maybe String -> String -> CF -> (String, String)
cf2SVAbs rp inPackage name cf = (mkHFile rp inPackage cab name cf, mkCFile inPackage cab name cf)
  where
    cab = cf2cabs cf

-- **** Header (.H) File Functions **** --

--Makes the Header file.
mkHFile :: RecordPositions -> Maybe String -> CAbs -> String -> CF -> String
mkHFile rp inPackage cabs name cf = unlines
 [
  "`ifndef " ++ (map toUpper name) ++ "_" ++ hdef,
  "`define " ++ (map toUpper name) ++ "_" ++ hdef,
  "",
  "typedef class Visitor;",
  "",
  "//************************************************************.",
  nsStart inPackage,
  "/********************   TypeDef Section    ********************/",
  "",
  unlines ["typedef " ++ d ++ " " ++ c ++ ";" | (c,d) <- svbasetypes],
  unlines ["typedef string " ++ s ++ ";" | s <- tokentypes cabs],
  "/********************   Forward Declarations    ********************/",
  "",
  unlines ["typedef class " ++ c ++ ";" | c <- classes, notElem c (defineds cabs)],
  "/********************   Visitor Interfaces    ********************/",
  prVisitor cabs,
  prVisitable,
  "/********************   Abstract Syntax Classes    ********************/",
  "",
  unlines [prAbs rp c | c <- absclasses cabs],
  unlines [prCon (c,r) | (c,rs) <- signatures cabs, r <- rs],
  unlines [prList c | c <- listtypes cabs],
  definedRules (Just $ LC nil cons) cf
  "/********************   Defined Constructors    ********************/",
  nsEnd inPackage,
  "`endif // " ++ (map toUpper name) ++ "_" ++ hdef
 ]
 where
  classes = allClasses cabs
  hdef = nsDefine inPackage "ABSYN_HEADER"
  nil  t = (,dummyType) $ concat [ "new List", identType t, "()" ]
  cons t = (,dummyType) $ concat [ "consList", identType t ]

-- auxiliaries

prVisitable :: String
prVisitable = unlines [
  "interface class Visitable;",
  "  pure virtual task accept(Visitor v);",
  "endclass"
  ]

prVisitor :: CAbs -> String
prVisitor cf = unlines [
  "interface class Visitor;",
  unlines
    ["  pure virtual task visit"++c++"("++c++" p);" | c <- allClasses cf,
                                                notElem c (defineds cf), notElem c (map fst (listtypes cf))],
  "",
  unlines
    ["  pure virtual task visit"++c++"("++c++" "++map toLower c ++");" | c <- map fst (listtypes cf)],
  unlines
    ["  pure virtual task visit"++c++"(" ++c++" x);" | c <- allNonClasses cf],
  "endclass"
 ]

prAbs :: RecordPositions -> String -> String
prAbs rp c = unlines [
  "virtual class " ++ c ++ " implements Visitable;",
  if rp == RecordPositions then "  int line_number;" else "",
  "  virtual task accept(Visitor v); endtask",
  "endclass"
  ]

prCon :: (String, CAbsRule) -> String
prCon (c,(f,cs)) = unlines [
  "class " ++ f ++ extends ++ c ++ ";",
  unlines
    ["  "++ typ +++ var ++ ";" | (typ,_,var) <- cs],
  "  extern function new" ++ "(" ++ conargs ++ ");",
  "  extern virtual task accept(Visitor v);",
  "endclass"
  ]
 where
   extends | c == "Visitable" = " implements "
           | otherwise = " extends "
   conargs = concat $ intersperse ", "
     [x +++ ("p" ++ show i) | ((x,_,_),i) <- zip cs [1..]]

prList :: (String,Bool) -> String
prList (c,b) = unlines [
  "class " ++ c ++ " implements Visitable;",
  "  " ++ bas ++ " v[$];",
  "  extern virtual task accept(Visitor v);",
  "endclass"
  ]
 where
   bas = drop 4 c {- drop "List" -} ++ if b then " " else ""


-- **** Implementation (.sv) File Functions **** --

mkCFile :: Maybe String -> CAbs -> String -> CF -> String
mkCFile inPackage cabs name _ = unlines $ [
  "`ifndef " ++ (map toUpper name) ++ "_ABSYN_SV",
  "`define " ++ (map toUpper name) ++ "_ABSYN_SV",
  "`include \"" ++ name ++ "/" ++ name ++ "Absyn.svh\"",
  nsStart inPackage,
  unlines [prConC  r | (_,rs) <- signatures cabs, r <- rs],
  unlines [prListC c | (c,_) <- listtypes cabs],
  nsEnd inPackage,
  "`endif"
  ]

prConC :: CAbsRule -> String
prConC fcs@(f,_) = unlines [
  "/********************   " ++ f ++ "    ********************/",
  prConstructorC fcs,
  prAcceptC f,
  ""
 ]

prListC :: String -> String
prListC c = unlines [
  "/********************   " ++ c ++ "    ********************/",
  "",
  prAcceptC c,
  ""
 ]


--The standard accept function for the Visitor pattern
prAcceptC :: String -> String
prAcceptC ty = unlines [
  "task " ++ ty ++ "::accept(Visitor v);",
  "  v.visit" ++ ty ++ "(this);",
  "endtask"
  ]

--The constructor assigns the parameters to the corresponding instance variables.
prConstructorC :: CAbsRule -> String
prConstructorC (f,cs) = unlines [
  "function " ++ f ++ "::new" ++ "(" ++ conargs ++ ");",
  unlines ["  " ++ c ++ " = " ++ p ++ ";" | (c,p) <- zip cvs pvs],
  "endfunction"
  ]
 where
   cvs = [c | (_,_,c) <- cs]
   pvs = ['p' : show i | ((_,_,_),i) <- zip cs [1..]]
   conargs = intercalate ", "
     [x +++ v | ((x,_,_),v) <- zip cs pvs]

-- | SV code for the @define@d constructors.
--
-- @definedRules Nothing@ only prints the header.
definedRules :: Maybe ListConstructors -> CF -> String -> String
definedRules mlc cf banner
  | null theLines = []
  | otherwise     = unlines $ banner : "" : theLines
  where
    theLines = map rule $ definitions cf

    ctx = buildContext cf

    rule (Define f args e t) =
      case mlc of
        Nothing -> header ++ ";"
        Just lc -> unlines
          [ "function " ++ header ++ ";"
          , svDecl lc e
          , "  automatic " ++ svType t ++ " p = " ++ svExp lc (map fst args) e ++ ";"
          , "  return p;"
          , "endfunction"
          ]
      where
        header = svType t ++ " " ++ sanitizeSV (funName f) ++ "_(" ++
                  intercalate ", " (map svArg args) ++ ")"

        svType :: Base -> String
        svType (ListT (BaseT x)) = "List" ++ x ++ " "
        svType (ListT t)         = svType t ++ " "
        svType (BaseT x)
            | x `elem` baseTokenCatNames = x
            | isToken x ctx = "String"
            | otherwise     = x ++ " "

        svArg :: (String, Base) -> String
        svArg (x,t) = svType t ++ " " ++ x ++ "_"

        svDecl lc exp =
          case exp of
            App x _ es
              | isUpper (head x) -> unlines $ map decl $ zip [0..] es
              | otherwise        -> error $ " //ERROR: " ++ show x -- ++ " :: " ++ show es
            _ -> ""
          where
          decl :: (Integer, Exp) -> String
          decl (idx, e) =
            case e of
              App y (FunT _ t) _
                | isUpper (head y) -> "  automatic " ++ show t ++ " p" ++ show idx ++ " = " ++ svExp lc [] e ++ ";"
                | otherwise        -> "//"
              _ -> ""

        svExp :: ListConstructors -> [String] -> Exp -> String
        svExp (LC nil cons) args = \d -> loop (-1, d)
          where
          loop :: (Integer, Exp) -> String
          loop = \case
            (_, App "[]"  (FunT [] (ListT t)) []) -> fst $ nil t
            (_, App "(:)" (FunT _  (ListT t)) es) -> call ("cons" ++ (fst $ cons t)) es
            (_, Var x)         -> x ++ "_"  -- argument
            (_, App t _ [e])
              | isToken t ctx  -> loop (-1, e)
            (i, App x _ es)
              | i >= 0           -> "p" ++ show i
              | isUpper (head x) -> call (x ++ "::new") es
              | x `elem` args    -> call (x ++ "_") es
              | otherwise        -> call (sanitizeSV x) es
            (_, LitInt n)      -> show n
            (_, LitDouble x)   -> show x
            (_, LitChar c)     -> show c
            (_, LitString s)   -> show s

          call x es = x ++ "(" ++ intercalate ", " (map loop (zip [0..] es)) ++ ")"

