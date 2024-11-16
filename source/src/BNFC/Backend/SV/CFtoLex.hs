{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.SV.CFtoLex
  ( cf2lex
  , preludeForBuffer  -- C code defining a buffer for lexing string literals.
  , svMacros          -- Lexer definitions.
  , commentStates     -- Stream of names for lexer states for comments.
  , lexComments       -- Lexing rules for comments.
  , lexStrings        -- Lexing rules for string literals.
  , lexChars          -- Lexing rules for character literals.
  ) where

import Prelude hiding                ( (<>) )
import Data.Bifunctor                ( first )
import Data.Char                     ( isAlphaNum, isAscii )
import Data.Maybe                    ( fromMaybe, maybeToList )
import qualified Data.Map as Map

import BNFC.CF
import BNFC.Backend.C.RegToFlex
import BNFC.Backend.Common.NamedVariables
import BNFC.PrettyPrint
import BNFC.Utils                    ( cstring, symbolToName, when )

-- | Entrypoint.
cf2lex :: Maybe String -> String -> CF -> (String, SymMap) -- The environment is reused by the parser.
cf2lex inPackage name cf = (, env) $ unlines
    [ prelude stringLiterals inPackage name
    , svMacros cf
    , lexSymbols env1
    , restOfLex inPackage cf env
    , footer -- mode
    ]
  where
    env  = Map.fromList env2
    env0 = makeSymEnv (cfgSymbols cf) [0 :: Int ..]
    env1 = env0 ++ makeKwEnv (reservedWords cf) [length env0 ..]
    env2 = map (first Keyword) env1 ++ map (\ x -> (Tokentype x, "T_" ++ x)) (tokenNames cf)
    makeSymEnv     = zipWith $ \ s n -> (s, '_' : fromMaybe ("SYMB_" ++ show n) (symbolToName s))
    makeKwEnv      = zipWith $ \ s n -> (s, "_KW_" ++ if all (\ c -> isAlphaNum c && isAscii c) s then s else show n)
    stringLiterals = isUsedCat cf (TokenCat catString)

prelude :: Bool -> Maybe String -> String -> String
prelude stringLiterals inPackage _ = unlines $ concat
  [ [ "/* Lexer definition for use with lex */"
    , ""
    , "/* This lex file was machine-generated */"
    , "%{"
    , "`define YY_BUFFER_LENGTH 4096"
    , ""
    , "string YY_PARSED_STRING;"
    , "task YY_BUFFER_APPEND(string s);"
    , "  s = {YY_PARSED_STRING, s}; //Do something better here!"
    , "endtask"
    , "task YY_BUFFER_RESET();"
    , "  for(int x = 0; x < `YY_BUFFER_LENGTH; x++)"
    , "    YY_PARSED_STRING[x] = 0;"
    , "endtask"
    , ""
    , "%}"
    , ""
    ]
  , when stringLiterals
    [ "/* Additional data for the lexer: a buffer for lexing string literals. */"
    , "%option extra-type=\"Buffer\""
    , ""
    ]
  , maybeToList $ ("%option prefix=\"" ++) . (++ "\"" ) <$> inPackage
  , [ "%{"
    , "int yy_mylinenumber = 0;"
    , "`define STRING_CAST(x)  string'(x)"
    , ""
    ]
  , [ "%}" ]
  , when stringLiterals $ preludeForBuffer $ "Buffer.svh"
    -- Flex is responsible for keeping tracking of the yylloc for Bison.
    -- Flex also doesn't do this automatically so we need this function
    -- https://stackoverflow.com/a/22125500/425756
  , [ "" ]
  ]

-- For now all categories are included.
-- Optimally only the ones that are used should be generated.
svMacros :: CF ->  String
svMacros cf = unlines
  [ "LETTER [a-zA-Z]"
  , "CAPITAL [A-Z]"
  , "SMALL [a-z]"
  , "DIGIT [0-9]"
  , "IDENT [a-zA-Z0-9'_]"
  , unwords $ concat
      [ [ "%START YYINITIAL CHAR CHARESC CHAREND STRING ESCAPED" ]
      , take (numberOfBlockCommentForms cf) commentStates
      ]
  , ""
  , "%%  /* Rules. */"
  ]

-- | Part of the lexer prelude needed when string literals are to be lexed.
--   Defines an interface to the Buffer.
preludeForBuffer :: String -> [String]
preludeForBuffer bufferH =
    [ "/* BEGIN extensible string buffer */"
    , ""
    , "`include \"" ++ bufferH ++ "\""
    , ""
    , "/* The initial size of the buffer to lex string literals. */"
    , "`define LITERAL_BUFFER_INITIAL_SIZE 1024"
    , ""
    , "/* The pointer to the literal buffer. */"
    , "`define literal_buffer yyextra"
    , ""
    , "/* Initialize the literal buffer. */"
    , "`define LITERAL_BUFFER_CREATE() literal_buffer = newBuffer(LITERAL_BUFFER_INITIAL_SIZE)"
    , ""
    , "/* Append characters at the end of the buffer. */"
    , "`define LITERAL_BUFFER_APPEND(s) bufferAppendString(literal_buffer, s)"
    , ""
    , "/* Append a character at the end of the buffer. */"
    , "`define LITERAL_BUFFER_APPEND_CHAR(c) bufferAppendChar(literal_buffer, c)"
    , ""
    , "/* Release the buffer, returning a pointer to its content. */"
    , "`define LITERAL_BUFFER_HARVEST() releaseBuffer(literal_buffer)"
    , ""
    , "/* In exceptional cases, e.g. when reaching EOF, we have to free the buffer. */"
    , "`define LITERAL_BUFFER_FREE() freeBuffer(literal_buffer)"
    , ""
    , "/* END extensible string buffer */"
    , ""
    ]

lexSymbols :: SymEnv -> String
lexSymbols ss = concatMap transSym ss
  where
    transSym (s,r) =
      "<YYINITIAL>\"" ++ s' ++ "\"      \t return `" ++ r ++ ";\n"
        where
         s' = escapeChars s

restOfLex :: Maybe String -> CF -> SymMap -> String
restOfLex inPackage cf env = unlines $ concat
  [ [ render $ lexComments inPackage (comments cf)
    , ""
    ]
  , userDefTokens
  , ifC catString  $ lexStrings "yylval" "`_STRING_" "`_ERROR_"
  , ifC catChar    $ lexChars   "yylval" "`_CHAR_"
  , ifC catDouble  [ "<YYINITIAL>{DIGIT}+\".\"{DIGIT}+(\"e\"(\\-)?{DIGIT}+)?      \t { automatic string str = `STRING_CAST(yytext); yylval._double = str.atoreal(); return `_DOUBLE_; }" ]
  , ifC catInteger [ "<YYINITIAL>{DIGIT}+      \t { automatic string str = `STRING_CAST(yytext); yylval._int = str.atoi(); return `_INTEGER_; }" ]
  , ifC catIdent   [ "<YYINITIAL>{LETTER}{IDENT}*      \t { yylval._string = `STRING_CAST(yytext); return `_IDENT_; }" ]
  , [ "\\n  ++" ++ "yy_mylinenumber ;\n"
    , "<YYINITIAL>[ \\t\\r\\n\\f]      \t /* ignore white space. */;"
    , "<YYINITIAL>.      \t return `_ERROR_;"
    , ""
    , "%%  /* Initialization code. */"
    , "task initialize_lexer(int inp); `BEGIN `YYINITIAL; endtask"
    ]
  ]
  where
  ifC cat s = if isUsedCat cf (TokenCat cat) then s else []
  userDefTokens =
    [ "<YYINITIAL>" ++ printRegFlex exp ++
       "    \t { yylval._string = `STRING_CAST(yytext); return `" ++ sName name ++ "; }"
    | (name, exp) <- tokenPragmas cf
    ]
    where sName n = fromMaybe n $ Map.lookup (Tokentype n) env

footer :: String
footer = unlines [ "" ]

-- | Lexing of strings, converting escaped characters.
lexStrings :: String -> String -> String -> [String]
lexStrings yylval stringToken errorToken =
    [ "<YYINITIAL>\"\\\"\"        \t { `LITERAL_BUFFER_CREATE(); `BEGIN `STRING; }"
    , "<STRING>\\\\             \t `BEGIN `ESCAPED;"
    , "<STRING>\\\"             \t { " ++ yylval ++ "._string = `LITERAL_BUFFER_HARVEST(); `BEGIN `YYINITIAL; return " ++ stringToken ++ "; }"
    , "<STRING>.              \t { `LITERAL_BUFFER_APPEND_CHAR(yytext[0]); }"
    , "<ESCAPED>f             \t { `LITERAL_BUFFER_APPEND_CHAR('\\f'); `BEGIN `STRING; }"
    , "<ESCAPED>n             \t { `LITERAL_BUFFER_APPEND_CHAR('\\n'); `BEGIN `STRING; }"
    , "<ESCAPED>r             \t { `LITERAL_BUFFER_APPEND_CHAR('\\r'); `BEGIN `STRING; }"
    , "<ESCAPED>t             \t { `LITERAL_BUFFER_APPEND_CHAR('\\t'); `BEGIN `STRING; }"
    , "<ESCAPED>\\\"            \t { `LITERAL_BUFFER_APPEND_CHAR('\"');  `BEGIN `STRING; }"
    , "<ESCAPED>\\\\            \t { `LITERAL_BUFFER_APPEND_CHAR('\\\\'); `BEGIN `STRING; }"
    , "<ESCAPED>.             \t { `LITERAL_BUFFER_APPEND(yytext);    `BEGIN `STRING; }"
    , "<STRING,ESCAPED><<EOF>>\t { `LITERAL_BUFFER_FREE(); return " ++ errorToken ++ "; }"
    ]

-- | Lexing of characters, converting escaped characters.
lexChars :: String -> String -> [String]
lexChars yylval charToken =
    [ "<YYINITIAL>\"'\" \t`BEGIN `CHAR;"
    , "<CHAR>\\\\      \t `BEGIN `CHARESC;"
    , "<CHAR>[^']      \t { `BEGIN `CHAREND; " ++ yylval ++ "._char = yytext[0]; return " ++ charToken ++ "; }"
    , "<CHARESC>f      \t { `BEGIN `CHAREND; " ++ yylval ++ "._char = \"\\f\";     return " ++ charToken ++ "; }"
    , "<CHARESC>n      \t { `BEGIN `CHAREND; " ++ yylval ++ "._char = \"\\n\";     return " ++ charToken ++ "; }"
    , "<CHARESC>r      \t { `BEGIN `CHAREND; " ++ yylval ++ "._char = \"\\r\";     return " ++ charToken ++ "; }"
    , "<CHARESC>t      \t { `BEGIN `CHAREND; " ++ yylval ++ "._char = \"\\t\";     return " ++ charToken ++ "; }"
    , "<CHARESC>.      \t { `BEGIN `CHAREND; " ++ yylval ++ "._char = yytext[0]; return " ++ charToken ++ "; }"
    , "<CHAREND>\"'\"      \t `BEGIN `YYINITIAL;"
    ]

-- ---------------------------------------------------------------------------
-- Comments

-- | Create flex rules for single-line and multi-lines comments.
-- The first argument is an optional namespace (for C++); the second
-- argument is the set of comment delimiters as returned by BNFC.CF.comments.
--
-- This function is only compiling the results of applying either
-- lexSingleComment or lexMultiComment on each comment delimiter or pair of
-- delimiters.
--
-- >>> lexComments (Just "myns.") ([("{-","-}")],["--"])
-- <YYINITIAL>"--"[^\n]*\n ++myns.yy_mylinenumber;
-- <YYINITIAL>"{-" `BEGIN `COMMENT;
-- <COMMENT>"-}" `BEGIN `YYINITIAL;
-- <COMMENT>. /* skip */;
-- <COMMENT>[\n] ++myns.yy_mylinenumber;
lexComments :: Maybe String -> ([(String, String)], [String]) -> Doc
lexComments ns (m,s) =
    vcat (map (lexSingleComment ns) s ++ map (lexMultiComment ns) m)

-- | If we have several block comments, we need different COMMENT lexing states.
commentStates :: [String]
commentStates = map ("COMMENT" ++) $ "" : map show [1::Int ..]

-- | Create a lexer rule for single-line comments.
-- The first argument is -- an optional c++ namespace
-- The second argument is the delimiter that marks the beginning of the
-- comment.
--
-- >>> lexSingleComment (Just "mypackage.") "--"
-- <YYINITIAL>"--"[^\n]*\n ++mypackage.yy_mylinenumber;
--
-- >>> lexSingleComment Nothing "--"
-- <YYINITIAL>"--"[^\n]*\n ++yy_mylinenumber;
--
-- >>> lexSingleComment Nothing "\""
-- <YYINITIAL>"\""[^\n]*\n ++yy_mylinenumber;
lexSingleComment :: Maybe String -> String -> Doc
lexSingleComment ns c =
    "<YYINITIAL>" <> cstring c <> "[^\\n]*\\n"
    <+> "++"<> text (fromMaybe "" ns)<>"yy_mylinenumber;"

-- | Create a lexer rule for multi-lines comments.
-- The first argument is -- an optional c++ namespace
-- The second arguments is the pair of delimiter for the multi-lines comment:
-- start deleminiter and end delimiter.
-- There might be a possible bug here if a language includes 2 multi-line
-- comments. They could possibly start a comment with one character and end it
-- with another.  However this seems rare.
--
-- >>> lexMultiComment Nothing ("{-", "-}")
-- <YYINITIAL>"{-" BEGIN COMMENT;
-- <COMMENT>"-}" BEGIN YYINITIAL;
-- <COMMENT>. /* skip */;
-- <COMMENT>[\n] ++yy_mylinenumber;
--
-- >>> lexMultiComment (Just "foo.") ("{-", "-}")
-- <YYINITIAL>"{-" BEGIN COMMENT;
-- <COMMENT>"-}" BEGIN YYINITIAL;
-- <COMMENT>. /* skip */;
-- <COMMENT>[\n] ++foo.yy_mylinenumber;
--
-- >>> lexMultiComment Nothing ("\"'", "'\"")
-- <YYINITIAL>"\"'" BEGIN COMMENT;
-- <COMMENT>"'\"" BEGIN YYINITIAL;
-- <COMMENT>. /* skip */;
-- <COMMENT>[\n] ++yy_mylinenumber;
lexMultiComment :: Maybe String -> (String, String) -> Doc
lexMultiComment ns (b,e) = vcat
    [ "<YYINITIAL>" <> cstring b <+> "`BEGIN `COMMENT;"
    , "<COMMENT>" <> cstring e <+> "`BEGIN `YYINITIAL;"
    , "<COMMENT>. /* skip */;"
    , "<COMMENT>[\\n] ++"<> text (fromMaybe "" ns) <>"yy_mylinenumber;"
    ]

--Helper function that escapes characters in strings
escapeChars :: String -> String
escapeChars [] = []
escapeChars ('\\':xs) = '\\' : ('\\' : (escapeChars xs))
escapeChars ('\"':xs) = '\\' : ('\"' : (escapeChars xs))
escapeChars (x:xs) = x : (escapeChars xs)
