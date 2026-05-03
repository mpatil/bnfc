{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.SV.PrettyPrinter.Visitors
  ( prDataH
  , prPrintData
  , prShowData
  ) where

import Prelude hiding ((<>))

import Data.Bifunctor (second)
import Data.Char (toLower)

import BNFC.Backend.Common
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.SV.PrettyPrinter.Render
import BNFC.Backend.SV.Utils
import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Utils

--Prints all the required method names and their parameters.
prDataH :: Bool -> (Cat, [Rule]) -> String
prDataH isShow (cat, rules)
 | isList cat = unlines $ concat
     [ [ concat ["  extern virtual task visit", cl, "(", cl, " ", vname, ");\n"] ]
     , when isShow
       [ concat ["  extern virtual task iter", cl, "(", cl, " l,", "int i, ", "int j);" ] ]
     ]
 | otherwise  = abstract ++ concatMap prRuleH rules
 where
   cl = identCat (normCat cat)
   abstract = case lookupRule (noPosition $ catToStr cat) rules of
    Just _ -> ""
    Nothing ->  "  extern virtual task visit" ++ cl ++ "(" ++ cl ++ " p); /* abstract class */\n"
   vname = map toLower cl

--Prints all the methods to visit a rule.
prRuleH :: IsFun f => Rul f -> String
prRuleH (Rule fun _ _ _) | isProperLabel fun = concat
  ["  extern virtual task visit", funName fun, "(", funName fun, " p);\n"]
prRuleH _ = ""

-- | Generates methods for the Pretty Printer.
prPrintData :: Maybe String -> CF -> (Cat, [Rule]) -> String
prPrintData _ _ (cat@(ListCat _), rules) =
    render $ genPrintVisitorList (cat, rules)
-- Not a list :
prPrintData _inPackage cf (TokenCat cat, _rules) | isPositionCat cf cat = unlines $
  -- a position token
  [ "task PrintAbsyn::visit" ++ cat ++ "(" ++ cat ++ " p);"
  , "  visitIdent(p.string_);"
  , "endtask"
  , ""
  ]
prPrintData inPackage _cf (cat, rules) = -- Not a list
    abstract ++ concatMap (prPrintRule inPackage) rules
  where
  cl = identCat (normCat cat)
  abstract = case lookupRule (noPosition $ catToStr cat) rules of
    Just _ -> ""
    Nothing ->  "task PrintAbsyn::visit" ++ cl ++ "(" ++ cl +++ "p); endtask //abstract class\n\n"

-- | Generate pretty printer visitor for a list category (STL version).
genPrintVisitorList :: (Cat, [Rule]) -> Doc
genPrintVisitorList (cat@(ListCat _), rules) = vcat
  [ "task PrintAbsyn::visit" <> lty <> parens (lty <+> vname) <> ";"
  , svtaskblock 2
    [ "iter" <> lty <> parens (vname <> comma <+> "0" <> comma <+> vname <> ".v.size()") <> semi ]
  , ""
  , "task PrintAbsyn::iter" <> lty <> parens (lty <> " l" <> comma <+> "int i" <> comma <+> "int j") <> ";"
  , svtaskblock 2 $ concat
    [ if null docs0 then
      [ "if (i == j) return;" ]
      else
      [ "if (i == j)"
      , "begin /* nil */"
      , nest 2 $ vcat docs0
      , "end"
      , "else"
      ]
    , unless (null docs1)
      [ "if (i == j-1)"
      , "begin /* last */"
      , nest 2 $ vcat docs1
      , "end"
      , "else"
      ]
    , [ "begin /* cons */"
      ,  (nest 2 $ vcat docs2)
      , "end"
      ]
    ]
  , ""
  , ""
  ]
  where
  cl        = identCat (normCat cat)
  lty       = text cl
  vname     = text $ map toLower cl
  prules    = sortRulesByPrecedence rules
  swRules f = switchByPrecedence "_i_" $
                map (second $ sep . prListRule_) $
                  uniqOn fst $ filter f prules
  docs0     = swRules isNilFun
  docs1     = swRules isOneFun
  docs2     = swRules isConsFun

genPrintVisitorList _ = error "genPrintVisitorList expects a ListCat"

-- | Only render the rhs (items) of a list rule.
prListRule_ :: IsFun a => Rul a -> [Doc]
prListRule_ (Rule _ _ items _) = for items $ \case
  Right t       -> "render(" <> text (snd (renderCharOrString t)) <> ");"
  Left c
    | Just{} <- maybeTokenCat c
                -> "visit" <> dat <> "(i);"
    | isList c  -> "iter" <> dat <> "(l, i+1, j);"
    | otherwise -> "l.v[i].accept(this);"
    where
    dat = text $ identCat $ normCat c

--Pretty Printer methods for a rule.
prPrintRule :: Maybe String -> Rule -> String
prPrintRule inPackage r@(Rule fun _ _ _) | isProperLabel fun = unlines $ concat
  [ [ "task PrintAbsyn::visit" ++ funName fun ++ "(" ++ funName fun +++ " " ++ fnm ++ ");"
    , ""
    , "  int oldi = _i_;"
    , parenCode "`_L_PAREN"
    , ""
    ]
  , prPrintRule_ (fnm ++ ".") r
  , [ ""
    , parenCode "`_R_PAREN"
    , "  _i_ = oldi;"
    , "endtask"
    , ""
    ]
  ]
  where
  p = precRule r
  parenCode x = "  if (oldi > " ++ show p ++ ") render(" ++ nsDefine inPackage x ++ ");"
  fnm = "p"
prPrintRule _ _ = ""

prPrintRule_ :: IsFun a => String -> Rul a -> [String]
prPrintRule_ pre (Rule _ _ items _) = map (prPrintItem pre) $ numVars items

--This goes on to recurse to the instance variables.
prPrintItem :: String -> Either (Cat, Doc) String -> String
prPrintItem _   (Right t) = "  render(" ++ snd (renderCharOrString t) ++ ");\n"
prPrintItem pre (Left (c, nt))
  | Just t <- maybeTokenCat c
              = "  visit" ++ t   ++ "(" ++ pre ++ s ++ ");\n"
  | isList c  = "  " ++ setI (precCat c) ++
                  "visit" ++ elt ++ "(" ++ pre ++ s ++ ");"
  | otherwise = "  " ++ setI (precCat c) ++ pre ++ s ++ ".accept(this);"
  where
  s   = render nt
  elt = identCat $ normCat c

--This prints the functions for Abstract Syntax tree printing.
prShowData :: (Cat, [Rule]) -> String
prShowData (cat@(ListCat c), _) = unlines
 [
  "task ShowAbsyn::visit" ++ cl ++ "("++ cl ++ " " ++ vname ++ ");",
  "",
  "  for ( int i = 0; i < " ++ vname ++ ".v.size() ; i++)",
  "  begin",
  if isTokenCat c
    then "    visit" ++ baseName cl ++ "(" ++ vname ++ ".v[i]) ;"
    else "    " ++ vname ++ ".v[i].accept(this);",
  "    if (i != " ++ vname ++ ".v.size() - 1) bufAppend(\", \");",
  "  end",
  "endtask"
 ]
  where
    cl = identCat (normCat cat)
    vname = map toLower cl

prShowData (cat, rules) =
  abstract ++ concatMap prShowRule rules
  where
    cl = identCat (normCat cat)
    abstract = case lookupRule (noPosition $ catToStr cat) rules of
      Just _ -> ""
      Nothing ->  "task ShowAbsyn::visit" ++ cl ++ "(" ++ cl ++ " p); endtask //abstract class\n\n"

--This prints all the methods for Abstract Syntax tree rules.
prShowRule :: IsFun f => Rul f -> String
prShowRule (Rule f _ cats _) | isProperLabel f = concat
  [
   "task ShowAbsyn::visit" ++ fun ++ "(" ++ fun +++ " " ++ fnm ++ ");\n",
   "\n",
   lparen,
   "  bufAppend(\"" ++ fun ++ "\");\n",
   optspace,
   cats',
   rparen,
   "endtask\n"
  ]
   where
    fun = funName f
    (optspace, lparen, rparen, cats')
      | null [ () | Left _ <- cats ]
                  = ("", "", "", "")
      | otherwise = ("  bufAppend(\"\\\"\");\n", "  bufAppend(\"(\");\n","  bufAppend(\")\");\n"
                    , concat (insertSpaces (map (prShowCat fnm) (numVars cats))))
    insertSpaces [] = []
    insertSpaces (x:[]) = [x]
    insertSpaces (x:xs) = if x == ""
      then insertSpaces xs
      else x : "  bufAppend(\" \");\n" : insertSpaces xs
    fnm = "p"
prShowRule _ = ""

-- This recurses to the instance variables of a class.
prShowCat :: String -> Either (Cat, Doc) String -> String
prShowCat _   (Right _) = ""
prShowCat fnm (Left (cat, nt))
  | Just t <- maybeTokenCat cat =
      unlines
        [ "  visit" ++ t ++ "(" ++ fnm ++ "." ++ s ++ ");\n"
        ]
  | catToStr (normCat $ strToCat s) /= s =
      unlines
        [ accept
        ]
  | otherwise =
      unlines
        [ "  bufAppend(\"[\");\n"
        , "  if (" ++ fnm ++ "." ++ s ++ ")" ++ accept
        , "  bufAppend(\"]\");\n"
        ]
  where
  s = render nt
  accept = "  " ++ fnm ++ "." ++ s ++ ".accept(this);\n"

baseName :: [a] -> [a]
baseName = drop 4

setI :: Integer -> String
setI n = "_i_ = " ++ show n ++ "; "
