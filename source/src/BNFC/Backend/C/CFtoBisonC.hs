{-
    BNF Converter: C Bison generator
    Copyright (C) 2004  Author:  Michael Pellauer

    Description   : This module generates the Bison input file.
                    Note that because of the way bison stores results
                    the programmer can increase performance by limiting
                    the number of entry points in their grammar.

    Author        : Michael Pellauer
    Created       : 6 August, 2003
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.C.CFtoBisonC
  ( cf2Bison
  , mkPointer
  , resultName, typeName, varName
  , specialToks, startSymbol
  , unionBuiltinTokens
  )
  where

import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import BNFC.CF
import BNFC.Backend.Common.NamedVariables hiding (varName)
import BNFC.Options (RecordPositions(..))
import BNFC.Utils ((+++))

--This follows the basic structure of CFtoHappy.

-- Type declarations
type Rules       = [(NonTerminal,[(Pattern,Action)])]
type Pattern     = String
type Action      = String
type MetaVar     = String

--The environment comes from the CFtoFlex
cf2Bison :: RecordPositions -> String -> CF -> SymMap -> String
cf2Bison rp name cf env = unlines
    [ header name cf
    , union (allParserCatsNorm cf)
    , unionDependentCode name
    , "%token _ERROR_"
    , tokens (map fst $ tokenPragmas cf) env
    , declarations cf
    , specialToks cf
    , startSymbol cf
    , ""
    , "%%"
    , ""
    , prRules (rulesForBison rp cf env)
    , "%%"
    , ""
    , entryCode name cf
    ]

header :: String -> CF -> String
header name cf = unlines
    [ "/* This Bison file was machine-generated by BNFC */"
    , ""
    , "/* Generate header file for lexer. */"
    , "%defines \"Bison.h\""
    , ""
    , "/* Reentrant parser */"
    , "%pure_parser"
         -- This flag is deprecated in Bison 3.7, but older Bisons don't recognize
         -- %define api.pure full
    , "%lex-param   { yyscan_t scanner }"
    , "%parse-param { yyscan_t scanner }"
    , ""
    , concat [ "/* Turn on line/column tracking in the ", name, "lloc structure: */" ]
    , "%locations"
    , ""
    , "/* Argument to the parser to be filled with the parsed tree. */"
    , "%parse-param { YYSTYPE *result }"
    , ""
    , "%{"
    , "/* Begin C preamble code */"
    , ""
    , "#include <stdlib.h>"
    , "#include <stdio.h>"
    , "#include <string.h>"
    , "#include \"Absyn.h\""
    , ""
    , "#define YYMAXDEPTH 10000000"  -- default maximum stack size is 10000, but right-recursion needs O(n) stack
    , ""
    , "/* The type yyscan_t is defined by flex, but we need it in the parser already. */"
    , "#ifndef YY_TYPEDEF_YY_SCANNER_T"
    , "#define YY_TYPEDEF_YY_SCANNER_T"
    , "typedef void* yyscan_t;"
    , "#endif"
    , ""
    , "typedef struct " ++ name ++ "_buffer_state *YY_BUFFER_STATE;"
    , "YY_BUFFER_STATE " ++ name ++ "_scan_string(const char *str, yyscan_t scanner);"
    , "void " ++ name ++ "_delete_buffer(YY_BUFFER_STATE buf, yyscan_t scanner);"
    , ""
    , "extern void " ++ name ++ "lex_destroy(yyscan_t scanner);"
    , "extern char* " ++ name ++ "get_text(yyscan_t scanner);"
    , ""
    , "extern yyscan_t " ++ name ++ "_init_lexer(FILE * inp);"
    , ""
    , "/* List reversal functions. */"
    , concatMap reverseList $ filter isList $ allParserCatsNorm cf
    , "/* End C preamble code */"
    , "%}"
    ]
  where
  eps = toList (allEntryPoints cf)
     -- Andreas, 2019-04-29, #210: Generate also parsers for CoercCat.
     -- WAS:  (allCatsNorm cf)
     -- Found old comment:
     -- -- M.F. 2004-09-17 changed allEntryPoints to allCatsIdNorm. Seems to fix the [Ty2] bug.

-- | Code that needs the @YYSTYPE@ defined by the @%union@ pragma.
--
unionDependentCode :: String -> String
unionDependentCode name = unlines
  [ "%{"
  , errorHandler name
  , "int yyparse(yyscan_t scanner, YYSTYPE *result);"
  , ""
  , "extern int yylex(YYSTYPE *lvalp, YYLTYPE *llocp, yyscan_t scanner);"
  , "%}"
  ]

errorHandler :: String -> String
errorHandler name = unlines
  [ "void yyerror(YYLTYPE *loc, yyscan_t scanner, YYSTYPE *result, const char *msg)"
  , "{"
  , "  fprintf(stderr, \"error: %d,%d: %s at %s\\n\","
  , "    loc->first_line, loc->first_column, msg, " ++ name ++ "get_text(scanner));"
  , "}"
  ]

-- | Parser entry point code.
--
entryCode :: String -> CF -> String
entryCode name cf = unlines $ map (parseMethod cf name) eps
  where
  eps = toList (allEntryPoints cf)

--This generates a parser method for each entry point.
parseMethod :: CF -> String -> Cat -> String
parseMethod cf name cat = unlines $ concat
  [ [ unwords [ "/* Entrypoint: parse", dat, "from file. */" ]
    , dat ++ " p" ++ parser ++ "(FILE *inp)"
    ]
  , body False
  , [ ""
    , unwords [ "/* Entrypoint: parse", dat, "from string. */" ]
    , dat ++ " ps" ++ parser ++ "(const char *str)"
    ]
  , body True
  ]
  where
  body stringParser = concat
    [ [ "{"
      , "  YYSTYPE result;"
      , "  yyscan_t scanner = " ++ name ++ "_init_lexer(", file, ");"
      , "  if (!scanner) {"
      , "    fprintf(stderr, \"Failed to initialize lexer.\\n\");"
      , "    return 0;"
      , "  }"
      ]
    , [ "  YY_BUFFER_STATE buf = " ++ name ++ "_scan_string(str, scanner);" | stringParser ]
    , [ "  int error = yyparse(scanner, &result);" ]
    , [ "  " ++ name ++ "_delete_buffer(buf, scanner);" | stringParser ]
    , [ "  " ++ name ++ "lex_destroy(scanner);"
      , "  if (error)"
      , "  { /* Failure */"
      , "    return 0;"
      , "  }"
      , "  else"
      , "  { /* Success */"
      , "    return" +++ res ++ ";"
      , "  }"
      , "}"
      ]
    ]
    where
    file | stringParser = "0"
         | otherwise    = "inp"
  ncat   = normCat cat
  dat    = identCat ncat
  parser = identCat cat
  res0   = concat [ "result.", varName ncat ]
  revRes = "reverse" ++ dat ++ "(" ++ res0 ++ ")"
  res    = if cat `elem` cfgReversibleCats cf then revRes else res0

--This method generates list reversal functions for each list type.
reverseList :: Cat -> String
reverseList c = unlines
 [
  c' ++ " reverse" ++ c' ++ "(" ++ c' +++ "l)",
  "{",
  "  " ++ c' +++"prev = 0;",
  "  " ++ c' +++"tmp = 0;",
  "  while (l)",
  "  {",
  "    tmp = l->" ++ v ++ ";",
  "    l->" ++ v +++ "= prev;",
  "    prev = l;",
  "    l = tmp;",
  "  }",
  "  return prev;",
  "}"
 ]
 where
  c' = identCat (normCat c)
  v = map toLower c' ++ "_"

--The union declaration is special to Bison/Yacc and gives the type of yylval.
--For efficiency, we may want to only include used categories here.
union :: [Cat] -> String
union cats = unlines $ concat
  [ [ "/* The type of a parse result (yylval). */" ]
  , [ "%union"
    , "{"
    ]
  , map ("  " ++) unionBuiltinTokens
  , concatMap mkPointer cats
  , [ "}"
    ]
  ]
--This is a little weird because people can make [Exp2] etc.
mkPointer :: Cat -> [String]
mkPointer c
  | identCat c /= show c  --list. add it even if it refers to a coercion.
    || normCat c == c     --normal cat
    = [ "  " ++ identCat (normCat c) +++ varName (normCat c) ++ ";" ]
  | otherwise = []

unionBuiltinTokens :: [String]
unionBuiltinTokens =
  [ "int    _int;"
  , "char   _char;"
  , "double _double;"
  , "char*  _string;"
  ]

--declares non-terminal types.
declarations :: CF -> String
declarations cf = concatMap (typeNT cf) (allParserCats cf)
 where --don't define internal rules
   typeNT cf nt | rulesForCat cf nt /= [] = "%type <" ++ varName (normCat nt) ++ "> " ++ identCat nt ++ "\n"
   typeNT _ _ = ""

--declares terminal types.
-- token name "literal"
-- "Syntax error messages passed to yyerror from the parser will reference the literal string instead of the token name."
-- https://www.gnu.org/software/bison/manual/html_node/Token-Decl.html
tokens :: [UserDef] -> SymMap -> String
tokens user env = unlines $ map declTok $ Map.toList env
 where
  declTok (Keyword   s, r) = tok "" s r
  declTok (Tokentype s, r) = tok (if s `elem` user then "<_string>" else "") s r
  tok t s r = "%token" ++ t ++ " " ++ r ++ "    /*   " ++ cStringEscape s ++ "   */"

-- | Escape characters inside a C string.
cStringEscape :: String -> String
cStringEscape = concatMap escChar
  where
    escChar c
      | c `elem` ("\"\\" :: String) = '\\':[c]
      | otherwise = [c]

specialToks :: CF -> String
specialToks cf = unlines $ concat
  [ ifC catString  "%token<_string> _STRING_"
  , ifC catChar    "%token<_char>   _CHAR_"
  , ifC catInteger "%token<_int>    _INTEGER_"
  , ifC catDouble  "%token<_double> _DOUBLE_"
  , ifC catIdent   "%token<_string> _IDENT_"
  ]
  where
    ifC cat s = if isUsedCat cf (TokenCat cat) then [s] else []

-- | Bison only supports a single entrypoint.
startSymbol :: CF -> String
startSymbol cf = "%start" +++ identCat (firstEntry cf)

--The following functions are a (relatively) straightforward translation
--of the ones in CFtoHappy.hs
rulesForBison :: RecordPositions -> CF -> SymMap -> Rules
rulesForBison rp cf env = map mkOne $ ruleGroups cf where
  mkOne (cat,rules) = constructRule rp cf env rules cat

-- For every non-terminal, we construct a set of rules.
constructRule
  :: RecordPositions -> CF -> SymMap
  -> [Rule]                           -- ^ List of alternatives for parsing ...
  -> NonTerminal                      -- ^ ... this non-terminal.
  -> (NonTerminal,[(Pattern,Action)])
constructRule rp cf env rules nt = (nt,) $
    [ (p,) $ addResult $ generateAction rp (identCat (normCat nt)) (funRule r) b m
    | r0 <- rules
    , let (b,r) = if isConsFun (funRule r0) && valCat r0 `elem` cfgReversibleCats cf
                  then (True, revSepListRule r0)
                  else (False, r0)
    , let (p,m) = generatePatterns cf env r
    ]
  where
    -- Add action if we parse an entrypoint non-terminal:
    -- Set field in result record to current parse.
    addResult a =
      if nt `elem` toList (allEntryPoints cf)
      -- Note: Bison has only a single entrypoint,
      -- but BNFC works around this by adding dedicated parse methods for all entrypoints.
      -- Andreas, 2021-03-24: But see #350: bison still uses only the @%start@ non-terminal.
        then concat [ a, " result->", varName (normCat nt), " = $$;" ]
        else a

-- | Generates a string containing the semantic action.
-- >>> generateAction NoRecordPositions "Foo" "Bar" False ["$1"]
-- "make_Bar($1);"
-- >>> generateAction NoRecordPositions "Foo" "_" False ["$1"]
-- "$1;"
-- >>> generateAction NoRecordPositions "ListFoo" "[]" False []
-- "0;"
-- >>> generateAction NoRecordPositions "ListFoo" "(:[])" False ["$1"]
-- "make_ListFoo($1, 0);"
-- >>> generateAction NoRecordPositions "ListFoo" "(:)" False ["$1","$2"]
-- "make_ListFoo($1, $2);"
-- >>> generateAction NoRecordPositions "ListFoo" "(:)" True ["$1","$2"]
-- "make_ListFoo($2, $1);"
generateAction :: IsFun a => RecordPositions -> String -> a -> Bool -> [MetaVar] -> Action
generateAction rp nt f b ms
  | isCoercion f = unwords ms ++ ";" ++ loc
  | isNilFun f   = "0;"
  | isOneFun f   = concat ["make_", nt, "(", intercalate ", " ms', ", 0);"]
  | isConsFun f  = concat ["make_", nt, "(", intercalate ", " ms', ");"]
  | otherwise    = concat ["make_", funName f, "(", intercalate ", " ms', ");", loc]
 where
  ms' = if b then reverse ms else ms
  loc = if rp == RecordPositions then " $$->line_number = @$.first_line; $$->char_number = @$.first_column;" else ""

-- Generate patterns and a set of metavariables indicating
-- where in the pattern the non-terminal
generatePatterns :: CF -> SymMap -> Rule -> (Pattern,[MetaVar])
generatePatterns cf env r = case rhsRule r of
  []  -> ("/* empty */",[])
  its -> (unwords (map mkIt its), metas its)
 where
   mkIt i = case i of
     Left (TokenCat s) -> fromMaybe (typeName s) $ Map.lookup (Tokentype s) env
     Left c  -> identCat c
     Right s -> fromMaybe s $ Map.lookup (Keyword s) env
   metas its = [revIf c ('$': show i) | (i,Left c) <- zip [1 :: Int ..] its]
   revIf c m = if not (isConsFun (funRule r)) && elem c revs
                 then "reverse" ++ identCat (normCat c) ++ "(" ++ m ++ ")"
               else m  -- no reversal in the left-recursive Cons rule itself
   revs = cfgReversibleCats cf

-- We have now constructed the patterns and actions,
-- so the only thing left is to merge them into one string.

prRules :: Rules -> String
prRules [] = []
prRules ((_, []):rs) = prRules rs --internal rule
prRules ((nt, (p,a) : ls):rs) =
  unwords [nt', ":" , p, "{ $$ =", a, "}", '\n' : pr ls] ++ ";\n" ++ prRules rs
 where
  nt' = identCat nt
  pr []           = []
  pr ((p,a):ls)   = unlines [unwords ["  |", p, "{ $$ =", a , "}"]] ++ pr ls

--Some helper functions.
resultName :: String -> String
resultName s = "YY_RESULT_" ++ s ++ "_"

-- | slightly stronger than the NamedVariable version.
-- >>> varName (Cat "Abc")
-- "abc_"
varName :: Cat -> String
varName = \case
  TokenCat s -> "_" ++ map toLower s
  c          -> (++ "_") . map toLower . identCat . normCat $ c

typeName :: String -> String
typeName "Ident" = "_IDENT_"
typeName "String" = "_STRING_"
typeName "Char" = "_CHAR_"
typeName "Integer" = "_INTEGER_"
typeName "Double" = "_DOUBLE_"
typeName x = x
