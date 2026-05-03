{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.SV.PrettyPrinter.Render
  ( prRender
  , svcodeblock
  , svtaskblock
  , renderCharOrString
  , escapeChars
  ) where

import Prelude hiding ((<>))

import Data.Char (ord)
import Text.Printf (printf)

import BNFC.PrettyPrint

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
