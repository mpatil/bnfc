{-# LANGUAGE NoImplicitPrelude #-}

module BNFC.Backend.SV.Testbench (svtest) where

import Prelude (String, (++), unlines)

import BNFC.Backend.SV.Config
import BNFC.CF

svtest :: CF -> SVConfig -> String
svtest cf cfg =
  unlines
   [
    "/****************************************************************************/",
    "/*                                                                          */",
    "/* This test will parse a file, print the abstract syntax tree, and then    */",
    "/* pretty-print the result.                                                 */",
    "/*                                                                          */",
    "/****************************************************************************/",
    "",
    "import " ++ svName cfg ++ "_pkg::*;",
    "",
    "program automatic test;",
    "",
    "  initial begin : prog",
    "    automatic string filename = \"test.w\";",
    "    automatic string test_str = \"\";",
    "    automatic " ++ def ++ " parse_tree;",
    "    automatic Interp i = new();",
    "    automatic PrintAbsyn p = new();",
    "    automatic ShowAbsyn q = new();",
    "",
    "    if ($value$plusargs(\"input=%s\", filename))",
    "      parse_tree = p" ++ dat ++ "(filename);",
    "    else",
    "      parse_tree = ps" ++ dat ++ "(test_str);",
    "    if (parse_tree) begin",
    "",
    "      p.print(parse_tree);",
    "      $write(\"%s\\n\", p.buf_);",
    "",
    "      q.show(parse_tree);",
    "      //$write(\"%s\\n\", q.buf_);",
    "",
    "      i.interpret(parse_tree);",
    "    end",
    "  end",
    "endprogram",
    ""
   ]
  where
   cat = firstEntry cf
   dat = identCat (normCat cat)
   def = identCat cat
