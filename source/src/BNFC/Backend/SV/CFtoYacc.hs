{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.SV.CFtoYacc
  ( cf2Yacc
  , resultName, typeName, varName
  , specialToks, startSymbol
  , unionBuiltinTokens
  )
  where

import Prelude hiding ((<>))

import Data.Char       ( toLower, isUpper)
import Data.Foldable   ( toList )
import Data.List       (nub, intercalate)
import qualified Data.Map as Map

import BNFC.CF
import BNFC.Backend.Common.NamedVariables hiding (varName)
import BNFC.PrettyPrint
import BNFC.Options (RecordPositions(..), InPackage)
import BNFC.Utils ((+++), table, applyWhen, for, when)

import BNFC.Backend.C.CFtoBisonC (startSymbol)

import BNFC.Backend.SV.Naming
import BNFC.Backend.SV.Utils

--This follows the basic structure of CFtoHappy.

-- Type declarations
type Rules       = [(NonTerminal,[(Pattern,Action)])]
type Pattern     = String
type Action      = String
type MetaVar     = String

cf2Yacc :: RecordPositions -> Maybe String -> String -> CF -> SymMap -> String
cf2Yacc rp inPackage name cf env = unlines
    [ header inPackage name cf
    , render $ union inPackage $ posCats ++ allParserCatsNorm cf
    , ""
    , unionDependentCode name
    , unlines $ table " " $ concat
      [ [ ["%token", "_ERROR_" ] ]
      , tokens user env
      , specialToks cf
      ]
    , declarations cf
    , startSymbol cf
    , "%{"
    , "//"
    , "%}"
    , ""
    , "%%"
    , prRules $ rulesForYacc rp inPackage cf env
    , "%%"
    , ""
    , "endclass"
    , ""
    , nsStart inPackage
    , entryCode inPackage name cf
    , nsEnd inPackage
    ]
  where
   user = map fst $ tokenPragmas cf
   posCats = map TokenCat $ positionCats cf

positionCats :: CF -> [String]
positionCats cf = [ wpThing name | TokenReg name True _ <- cfgPragmas cf ]

header :: Maybe String -> String -> CF -> String
header inPackage _ cf = unlines
    [ "/*************************************************/"
    , "%{"
    , "`include \"Absyn.svh\""
    , "`include \"bio.svh\""
    , ""
    , "class Parser;"
    , "  Biobuf b;"
    , "`include \"Lexer.svh\""
    , ""
    , "typedef struct { int i; } YY_BUFFER_STATE;"
    , ""
    , "function int " ++ ns ++ "yywrap();"
    , "  return 1;"
    , "endfunction"
    , ""
    , nsStart inPackage
    , unlines $ map parseResult dats
    , nsEnd inPackage
    , "%}"
    ]
  where
    ns   = nsString inPackage
    eps  = toList (allEntryPoints cf) ++ map TokenCat (positionCats cf)
    dats = nub $ map normCat eps

-- | Code that needs the @YYSTYPE@ defined by the @%union@ pragma.
--
unionDependentCode :: String -> String
unionDependentCode name = unlines
  [ "%{"
  , errorHandler name
  , ""
  , "%}"
  ]

errorHandler :: String -> String
errorHandler _ = unlines
  [ "task yyerror(string str);"
  , "  $display(\"error: line %0d: %s at %s\\n\", yy_mylinenumber, str, "  ++ "string'(yytext)); //'"
  , "  $fatal;"
  , "endtask"
  , ""
  , "task execerror(string s, string t);   /* recover from run-time error */"
  , "  if (s != \"\") $write(\" %s \", s);"
  , "    if (t != \"\") $write(\" %s \", t);"
  , "    $fatal (1, $psprintf(\"\\nFATAL ERROR: line %0d near \\\"%s\\\": exiting!!!\\n\\n\", yy_mylinenumber, string'(yytext))); //'"
  , "endtask"
  , ""
  ]

-- | Parser entry point code.
--
entryCode :: Maybe String -> String -> CF -> String
entryCode inPackage name cf = unlines $ map (parseMethod inPackage name cf) eps
  where
  eps = toList (allEntryPoints cf)

-- | Generates declaration and initialization of the @YY_RESULT@ for a parser.
--
--   Different parsers (for different precedences of the same category)
--   share such a declaration.
--
--   Expects a normalized category.
parseResult :: Cat -> String
parseResult cat = 
  "static " ++ cat' ++ " " +++ resultName cat' +++ "= null;"
  where
  cat' = identCat cat

--This generates a parser method for each entry point.
parseMethod :: Maybe String -> String -> CF -> Cat -> String
parseMethod _ _ cf cat = unlines $ concat
  [ [ unwords [ "/* Entrypoint: parse", dat, "from file. */" ]
    , "function " ++ cat' ++ " p" ++ parser ++ "(string filename);"
    ]
  , body False
  , [ "endfunction" ]
  , [ ""
    , unwords [ "/* Entrypoint: parse", dat, "from string. */" ]
    , "function " ++ cat' ++ " ps" ++ parser ++ "(string str);"
    ]
  , body True
  , [ "endfunction" ]
  ]
  where
  cat' = identCat (normCat cat)
  body stringParser = concat
    [ [ "automatic Parser p = new();"
      , if stringParser then "  p.b = Bopens(str);" else "  p.b = Bopen(filename, `OREAD);"
      , "  p.yy_mylinenumber = 1;"
      , "  p.initialize_lexer(0);"
      , "  if (p.yyparse())"
      , "    return null; /* Failure */"
      , "  else"
      , "    return p." ++ resultName cat' ++ ";/* Success */"
      ]
    ]
  ncat   = normCat cat
  dat0   = identCat ncat
  dat    = dat0
  parser = identCat cat

-- | The union declaration is special to Yacc and gives the type of
-- yylval.  For efficiency, we may want to only include used categories here.
--
-- >>> let foo = Cat "Foo"
-- >>> union Nothing [foo, ListCat foo]
-- %union
-- {
--   int int_;
--   char char_;
--   double double_;
--   string string_;
--   Foo foo_;
--   ListFoo* listfoo_;
-- }
--
-- If the given list of categories is contains coerced categories, those should
-- be normalized and duplicate removed
-- E.g. if there is both [Foo] and [Foo2] we should only print one pointer:
--    ListFoo listfoo_;
--
-- >>> let foo2 = CoercCat "Foo" 2
-- >>> union Nothing [foo, ListCat foo, foo2, ListCat foo2]
-- %union
-- {
--   int int_;
--   char char_;
--   double double_;
--   string string_;
--   Foo foo_;
--   ListFoo listfoo_;
-- }
union :: Maybe String -> [Cat] -> Doc
union inPackage cats = vcat
    [ "%union"
    , codeblock 2 $ map text unionBuiltinTokens ++ map mkPointer normCats
    ]
  where
    normCats = nub (map normCat cats)
    mkPointer s = scope <> text (identCat s) <> " " <+> text (varName s) <> ";"
    scope = text $ nsScope inPackage

unionBuiltinTokens :: [String]
unionBuiltinTokens =
  [ "int    _int;"
  , "byte   _char;"
  , "real   _double;"
  , "string _string;"
  ]

--declares non-terminal types.
declarations :: CF -> String
declarations cf = concatMap typeNT $
  map TokenCat (positionCats cf) ++
  filter (not . null . rulesForCat cf) (allParserCats cf) -- don't define internal rules
  where
  typeNT nt = "%type <" ++ varName nt ++ "> " ++ identCat nt ++ "\n"

--declares terminal types.
-- token name "literal"
-- "Syntax error messages passed to yyerror from the parser will reference the literal string instead of the token name."
-- https://www.gnu.org/software/bison/manual/html_node/Token-Decl.html
tokens :: [UserDef] -> SymMap -> [[String]]
tokens user env = map declTok $ Map.toList env
  where
  declTok (Keyword   s, r) = tok "" s r
  declTok (Tokentype s, r) = tok (if s `elem` user then "<_string>" else "") s r
  tok t s r = [ "%token" ++ t, r, " /* " ++ cStringEscape s ++ " */" ]

-- | Escape characters inside a C string.
cStringEscape :: String -> String
cStringEscape = concatMap escChar
  where
    escChar c
      | c `elem` ("\"\\" :: String) = '\\':[c]
      | otherwise = [c]

-- | Produces a table with the built-in token types.
specialToks :: CF -> [[String]]
specialToks cf = concat
  [ ifC catString  [ "%token<_string>", "_STRING_"  ]
  , ifC catChar    [ "%token<_char>  ", "_CHAR_"    ]
  , ifC catInteger [ "%token<_int>   ", "_INTEGER_" ]
  , ifC catDouble  [ "%token<_double>", "_DOUBLE_"  ]
  , ifC catIdent   [ "%token<_string>", "_IDENT_"   ]
  ]
  where
    ifC cat s = if isUsedCat cf (TokenCat cat) then [s] else []

--The following functions are a (relatively) straightforward translation
--of the ones in CFtoHappy.hs
rulesForYacc rp inPackage cf env = map mkOne (ruleGroups cf) ++ posRules
  where
  mkOne (cat,rules) = constructRule rp inPackage cf env rules cat
  posRules :: Rules
  posRules = for (positionCats cf) $ \ n -> (TokenCat n,
      [( Map.findWithDefault n (Tokentype n) env
       , addResult cf (TokenCat n) $ concat
         [ "$$ = ", nsScope inPackage, n, "::new ", "($1, ", nsString inPackage, "yy_mylinenumber) ; YY_RESULT_" , n , "_= $$ ;" ]
       )])

-- For every non-terminal, we construct a set of rules.
constructRule ::
  RecordPositions -> Maybe String -> CF -> SymMap -> [Rule] -> NonTerminal -> (NonTerminal,[(Pattern,Action)])
constructRule rp inPackage cf env rules nt = (nt,) $
    [ (p,) $  addResult cf nt $ generateAction rp inPackage (identCat (normCat nt)) (funRule r) b m
    | r0 <- rules
    , let (b,r) = if isConsFun (funRule r0) && (valCat r0) `elem` cfgReversibleCats cf
                  then (True,revSepListRule r0)
                  else (False,r0)
    , let (p,m) = generatePatterns cf env r b
    ]

-- | Add action if we parse an entrypoint non-terminal:
-- Set field in result record to current parse.
addResult :: CF -> NonTerminal -> Action -> Action
addResult cf nt a =
  if nt `elem` toList (allEntryPoints cf)
  -- Note: Bison has only a single entrypoint,
  -- but BNFC works around this by adding dedicated parse methods for all entrypoints.
  -- Andreas, 2021-03-24: But see #350: bison still uses only the @%start@ non-terminal.
    then concat [ a, resultName (identCat nt), " = $$;" ]
    else a

generateAction :: IsFun a => RecordPositions -> InPackage -> String -> a -> Bool -> [(MetaVar,Bool)] -> Action
generateAction rp inPackage nt f b mbs = reverses ++
  if | isCoercion f    -> concat ["$$ = ", unwords ms, ";", loc]
     | isNilFun f      -> concat ["$$ = ", scope, nt, "::new();"]
     | isOneFun f      -> concat ["$$ = ", scope, nt, "::new(); $$.v.push_back(", head ms, ");"]
     | isConsFun f     -> concat [lst, ".v.push_back(", el, "); $$ = ", lst, ";"]
     | isDefinedRule f -> concat ["$$ = ", scope, sanitizeSV (funName f), "_", "(", intercalate ", " ms, ");" ]
     | otherwise       -> concat ["$$ = ", scope, funName f, "::new ", "(", intercalate ", " ms, ");", loc]
 where
  ms        = map fst mbs
  -- The following match only happens in the cons case:
  [el, lst] = applyWhen b reverse ms  -- b: left-recursion transformed?

  loc | RecordPositions <- rp
            = " $$.line_number = " ++ nsString inPackage ++ "yy_mylinenumber;"
      | otherwise
            = ""
  reverses = unwords [ m ++ ".v.reverse ;" | (m,True) <- mbs]
  scope     = nsScope inPackage
  new :: String -> String
  new  = \ s -> if isUpper (head s) then "::new " ++ s else sanitizeSV s

-- Generate patterns and a set of metavariables indicating
-- where in the pattern the non-terminal
generatePatterns :: CF -> SymMap -> Rule -> Bool -> (Pattern,[(MetaVar,Bool)])
generatePatterns cf env r _ = case rhsRule r of
  []  -> ("/* empty */",[])
  its -> (unwords (map mkIt its), metas its)
 where
   mkIt = \case
     Left (TokenCat s)
       | isPositionCat cf s -> typeName s
       | otherwise -> Map.findWithDefault (typeName s) (Tokentype s) env
     Left c  -> identCat c
     Right s -> Map.findWithDefault s (Keyword s) env
   metas its = [('$': show i,revert c) | (i,Left c) <- zip [1 :: Int ..] its]
   -- notice: reversibility with push_back vectors is the opposite
   -- of right-recursive lists!
   revert c = isntCons && isList c && notElem c revs
   revs     = cfgReversibleCats cf
   isntCons = not $ isConsFun $ funRule r

-- We have now constructed the patterns and actions,
-- so the only thing left is to merge them into one string.

prRules :: Rules -> String
prRules [] = []
prRules ((_, []):rs) = prRules rs --internal rule
prRules ((nt, (p, a) : ls):rs) =
    unwords [nt', ":" , p, "{ ", a, "}", "\n" ++ pr ls] ++ ";\n" ++ prRules rs
 where
  nt' = identCat nt
  pr []           = []
  pr ((p,a):ls)   = unlines [unwords ["  |", p, "{ ", a , "}"]] ++ pr ls

--Some helper functions.
resultName :: String -> String
resultName s = "YY_RESULT_" ++ s ++ "_"

--slightly stronger than the NamedVariable version.
varName :: Cat -> String
varName = (++ "_") . map toLower . identCat . normCat

typeName :: String -> String
typeName "Ident" = "_IDENT_"
typeName "String" = "_STRING_"
typeName "Char" = "_CHAR_"
typeName "Integer" = "_INTEGER_"
typeName "Double" = "_DOUBLE_"
typeName x = x
