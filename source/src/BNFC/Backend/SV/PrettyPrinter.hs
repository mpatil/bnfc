{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.SV.PrettyPrinter (cf2SVPrinter, prRender) where

import Prelude hiding ((<>))

import Data.Bifunctor (second)
import Data.Char (toLower, toUpper, ord)
import Text.Printf

import BNFC.CF
import BNFC.Utils
import BNFC.Backend.Common
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.SV.Utils
import BNFC.PrettyPrint

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

{- **** Header (.h) File Methods **** -}

--An extremely large function to make the Header File
mkHFile :: Maybe String -> String-> CF -> [(Cat,[Rule])] -> String
mkHFile inPackage name cf groups = unlines
  [ printHeader
  , contentPrint
  , classFooter
  , showHeader
  , contentShow
  , classFooter
  , footer
  ]
  where
  printHeader = unlines
   [
    "`ifndef " ++ (map toUpper name) ++ "_" ++ hdef,
    "`define " ++ (map toUpper name) ++ "_" ++ hdef,
    "",
    "`include \"" ++ name ++ "/" ++ (map toUpper name) ++ "Absyn.svh\"",
    nsStart inPackage,
    "/* Certain applications may improve performance by changing the buffer size */",
    "`define " ++ nsDefine inPackage "BUFFER_INITIAL" ++ " 2000",
    "/* You may wish to change _L_PAREN or _R_PAREN */",
    "`define " ++ nsDefine inPackage "_L_PAREN" ++ " \"(\"",
    "`define " ++ nsDefine inPackage "_R_PAREN" ++ " \")\"",
    "",
    "class PrintAbsyn implements Visitor;",
    " ",
    "  int _n_, _i_;",
    "  /* The following are simple heuristics for rendering terminals */",
    "  /* You may wish to change them */",
    "  extern task render(string c);",
    "  extern task render_s(string s);",
    "  extern task indent();",
    "  extern task backup();",
    " ",
    "  extern function new ();",
    " ",
    "  extern task print(Visitable v);"
   ]
  hdef = nsDefine inPackage "PRINTER_HEADER"
  contentShow = concatMap (prDataH False) groups
  contentPrint = concatMap (prDataH True) groups
  classFooter = unlines $
   [
    "  extern virtual task visitInteger(Integer x);",
    "  extern virtual task visitDouble(Double x);",
    "  extern virtual task visitChar(Char x);",
    "  extern virtual task visitString(string x);",
    "  extern virtual task visitIdent(string x);"
   ] ++ ["  extern virtual task visit" ++ t ++ "(" ++ t ++ " x);" | t <- tokenNames cf] ++
   [
    " ",
    "  string buf_;",
    "",
    "  task bufAppend(string s);",
    " ",
    "    buf_ = {buf_, s};",
    "  endtask",
    "",
    "  task bufReset();",
    " ",
    "    buf_ = \"\";",
    "  endtask",
    "",
    "endclass"
   ]
  showHeader = unlines
   [
    "",
    "class ShowAbsyn implements Visitor;",
    " ",
    "  extern function new();",
    "  extern task show(Visitable v);"
   ]
  footer = unlines
   [
    nsEnd inPackage,
    "`endif"
   ]

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

{- **** Implementation (.sv) File Methods **** -}

--This makes the .sv file by a similar method.
mkCFile :: Maybe String -> String -> CF -> [(Cat,[Rule])] -> String
mkCFile inPackage name cf groups = concat
   [
    header,
    nsStart inPackage ++ "\n",
    prRender,
    printEntries,
    concatMap (prPrintData inPackage cf) groups,
    printBasics,
    printTokens,
    showEntries,
    concatMap (prShowData) groups,
    showBasics,
    showTokens,
    nsEnd inPackage ++ "\n"
   ]
  where
    header = unlines
     [
      "/*** Generated Pretty Printer and Abstract Syntax Viewer ***/",
      "",
      "`include \"" ++ name ++ "/" ++ (map toUpper name) ++ "Printer.svh\"",
      "`define INDENT_WIDTH 2",
      ""
     ]
    printEntries = unlines
     [
      "function PrintAbsyn::new();",
      "",
      "  _i_ = 0; _n_ = 0;",
      "  buf_ = \"\";",
      "endfunction",
      "",
      "task PrintAbsyn::print(Visitable v);",
      "",
      "  _i_ = 0; _n_ = 0;",
      "  buf_=\"\";",
      "  v.accept(this);",
      "endtask",
      ""
     ]
    showEntries = unlines
     [
      "function ShowAbsyn::new();",
      "  buf_ = \"\";",
      "endfunction",
      "",
      "",
      "task ShowAbsyn::show(Visitable v);",
      "",
      "  buf_=\"\";",
      "  v.accept(this);",
      "endtask",
      ""
     ]
    printBasics = unlines
     [
      "task PrintAbsyn::visitInteger(Integer x);",
      "",
      "  string tmp = $psprintf(\"%d\", x);",
      "  bufAppend(tmp);",
      "endtask",
      "",
      "task PrintAbsyn::visitDouble(Double x);",
      "",
      "  string tmp = $psprintf(\"%g\", x);",
      "  bufAppend(tmp);",
      "endtask",
      "",
      "task PrintAbsyn::visitChar(Char x);",
      "",
      "  bufAppend(\"\\\"\");",
      "  bufAppend(x);",
      "  bufAppend(\"\\\"\");",
      "endtask",
      "",
      "task PrintAbsyn::visitString(string x);",
      "",
      "  bufAppend(\"\\\"\");",
      "  bufAppend(x);",
      "  bufAppend(\"\\\"\");",
      "endtask",
      "",
      "task PrintAbsyn::visitIdent(string x);",
      "",
      "  render(x);",
      "endtask",
      ""
     ]

    printTokens = unlines
     [unlines [
      "task PrintAbsyn::visit" ++ t ++ "(" ++ t ++ " x);",
      "",
      "  render(x);",
      "endtask",
      ""
      ] | t <- tokenNames cf
     ]

    showBasics = unlines
     [
      "task ShowAbsyn::visitInteger(Integer x);",
      "",
      "  string tmp = $psprintf(\"%d\", x);",
      "  bufAppend(tmp);",
      "endtask",
      "",
      "task ShowAbsyn::visitDouble(Double x);",
      "  string tmp = $psprintf(\"%g\", x);",
      "  bufAppend(tmp);",
      "endtask",
      "",
      "task ShowAbsyn::visitChar(Char x);",
      "  bufAppend(\"\\\"\");",
      "  bufAppend(x);",
      "  bufAppend(\"\\\"\");",
      "endtask",
      "",
      "task ShowAbsyn::visitString(string x);",
      "  bufAppend(\"\\\"\");",
      "  bufAppend(x);",
      "  bufAppend(\"\\\"\");",
      "endtask",
      "",
      "task ShowAbsyn::visitIdent(string x);",
      "  bufAppend(\"\\\"\");",
      "  bufAppend(x);",
      "  bufAppend(\"\\\"\");",
      "endtask",
      ""
     ]

    showTokens = unlines
     [unlines [
      "task ShowAbsyn::visit" ++ t ++ "(" ++ t ++ " x);",
      "",
      "  bufAppend(\"\\\"\");",
      "  bufAppend(x);",
      "  bufAppend(\"\\\"\");",
      "endtask",
      ""
      ] | t <- tokenNames cf
     ]


{- **** Pretty Printer Methods **** -}

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
--
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
  lty       = text cl                   -- List type
  vname     = text $ map toLower cl
  prules    = sortRulesByPrecedence rules
  swRules f = switchByPrecedence "_i_" $
                map (second $ sep . prListRule_) $
                  uniqOn fst $ filter f prules
                  -- Discard duplicates, can only handle one rule per precedence.
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
  fnm = "p" --old names could cause conflicts
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

{- **** Abstract Syntax Tree Printer **** -}

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

prShowData (cat, rules) =  --Not a list:
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
      | null [ () | Left _ <- cats ]  -- @all isRight cats@, but Data.Either.isRight requires base >= 4.7
                  = ("", "", "", "")
      | otherwise = ("  bufAppend(\"\\\"\");\n", "  bufAppend(\"(\");\n","  bufAppend(\")\");\n"
                    , concat (insertSpaces (map (prShowCat fnm) (numVars cats))))
    insertSpaces [] = []
    insertSpaces (x:[]) = [x]
    insertSpaces (x:xs) = if x == ""
      then insertSpaces xs
      else x : "  bufAppend(\" \");\n" : insertSpaces xs
    fnm = "p" --other names could cause conflicts
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

{- **** Helper Functions Section **** -}

-- from ListIdent to Ident
baseName :: [a] -> [a]
baseName = drop 4

--Just sets the coercion level for parentheses in the Pretty Printer.
setI :: Integer -> String
setI n = "_i_ = " ++ show n ++ "; "

--An extremely simple renderer for terminals.
prRender :: String
prRender = unlines $ concat
  [ [
      "//You may wish to change render",
      "task PrintAbsyn::render(string c);",
      "",
      "  if (c == \"{\")",
      "  begin",
      "     bufAppend(\"\\n\");",
      "     indent();",
      "     bufAppend(c);",
      "     _n_ = _n_ + `INDENT_WIDTH;",
      "     bufAppend(\"\\n\");",
      "     indent();",
      "  end",
      "  else if (c == \"(\" || c == \"[\")",
      "     bufAppend(c);",
      "  else if (c == \")\" || c == \"]\")",
      "  begin",
      "     backup();",
      "     bufAppend(c);",
      "  end",
      "  else if (c == \"}\")",
      "  begin",
      "     int t;",
      "     _n_ = _n_ - `INDENT_WIDTH;",
      "     for (t=0; t<`INDENT_WIDTH; t++) begin",
      "       backup();",
      "     end",
      "     bufAppend(c);",
      "     bufAppend(\"\\n\");",
      "     indent();",
      "  end",
      "  else if (c == \",\")",
      "  begin",
      "     backup();",
      "     bufAppend(c);",
      "     bufAppend(\" \");",
      "  end",
      "  else if (c == \";\")",
      "  begin",
      "     backup();",
      "     bufAppend(c);",
      "     bufAppend(\"\\n\");",
      "     indent();",
      "  end",
      "  else if (c == \" \") bufAppend(c);",
      "  else if (c == 0) return;",
      "  else",
      "  begin",
      "     bufAppend(\" \");",
      "     bufAppend(c);",
      "     bufAppend(\" \");",
      "  end",
      "endtask",
      "",
      "task PrintAbsyn::render_s(string s);",
      "",
      "  if(s.len() > 0)",
      "  begin",
      "    bufAppend(s);",
      "    bufAppend(\" \");",
      "  end",
      "endtask",
      "",
      "task PrintAbsyn::indent();",
      "",
      "  int n = _n_;",
      "  while (n > 0)",
      "  begin",
      "    bufAppend(\" \");",
      "    n--;",
      "  end",
      "endtask",
      "",
      "task PrintAbsyn::backup();",
      "/*",
      "  if (buf_[cur_ - 1] == \" \")",
      "  begin",
      "    buf_[cur_ - 1] = 0;",
      "    cur_--;",
      "  end",
      "*/",
      "endtask",
      ""
    ]
  ]

svcodeblock :: Int -> [Doc] -> Doc
svcodeblock indent code = "begin" $+$ nest indent (vcat code) $+$ "end"

svtaskblock :: Int -> [Doc] -> Doc
svtaskblock indent code = "" $+$ nest indent (vcat code) $+$ "endtask"

renderCharOrString :: String -> (Char, String)
renderCharOrString [c] | ord c <= 255 = ('C', printf "\"%c\"" c)
renderCharOrString s = ('S', printf "\"%s\"" (escapeChars s))

escapeChars :: String -> String
escapeChars [] = []
escapeChars ('\\':xs) = '\\' : '\\' : escapeChars xs
escapeChars ('\"':xs) = '\\' : '\"' : escapeChars xs
escapeChars ('\'':xs) = '\\' : '\'' : escapeChars xs
escapeChars (x:xs) = x : escapeChars xs
