{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.SV (makeSV,) where

import Prelude hiding ((<>))

import Data.Char (toUpper)
import Data.Foldable (toList)

import BNFC.Utils
import BNFC.CF
import BNFC.Options
import BNFC.Backend.Base
import qualified BNFC.Backend.Common.Makefile as Makefile

import BNFC.Backend.SV.Makefile
import BNFC.Backend.SV.CFtoAbs
import BNFC.Backend.SV.CFtoLex
import BNFC.Backend.SV.CFtoYacc
import BNFC.Backend.SV.CFtoInterp
import BNFC.Backend.SV.PrettyPrinter
import BNFC.Backend.SV.Utils

makeSV :: SharedOptions -> CF -> MkFiles ()
makeSV opts cf = do
    let (hfile, cfile) = cf2SVAbs (linenumbers opts) (inPackage opts) name cf
    mkfile (name ++ "/" ++ name ++ "Absyn.svh") commentWithEmacsModeHint hfile
    mkfile (name ++ "/" ++ name ++ "Absyn.sv") commentWithEmacsModeHint cfile
    let (flex, env) = cf2lex (inPackage opts) name cf
    mkfile (name ++ ".l") commentWithEmacsModeHint flex
    let bison = cf2Yacc (linenumbers opts) (inPackage opts) name cf env
    mkfile (name ++ ".y") commentWithEmacsModeHint bison
    let (skelH, skelC) = cf2Interp (inPackage opts) name cf
    mkfile (name ++ "/" ++ name ++ "Interp.svh") commentWithEmacsModeHint skelH
    mkfile (name ++ "/" ++ name ++ "Interp.sv") commentWithEmacsModeHint skelC
    let (prinH, prinC) = cf2SVPrinter True (inPackage opts) name cf
    mkfile (name ++ "/" ++ name ++ "Printer.svh") commentWithEmacsModeHint prinH
    mkfile (name ++ "/" ++ name ++ "Printer.sv") commentWithEmacsModeHint prinC
    mkfile "Test.sv" commentWithEmacsModeHint (svtest cf name)
    Makefile.mkMakefile (optMake opts) $ makefile name prefix
    mkfile (name ++ "/" ++ name ++ "_pkg.sv") commentWithEmacsModeHint (svpkg name)
    mkfile (name ++ "/" ++ name ++ ".core") ("CAPI=2:\n# " ++) (fusesoc name)
  where
    name :: String
    name = lang opts
    -- The prefix is a string used by flex and bison
    -- that is prepended to generated function names.
    -- It should be a valid C identifier.
    prefix :: String
    prefix = snakeCase_ name ++ "_"

-- | Put string into a block comment.
comment :: String -> String
comment x = unwords ["/*", x, "*/"]

-- | C line comment including mode hint for emacs.
commentWithEmacsModeHint :: String -> String
commentWithEmacsModeHint = comment . ("-*- sv -*- " ++)

svtest cf name =
  unlines
   [
    "/****************************************************************************/",
    "/*                                                                          */",
    "/* This test will parse a file, print the abstract syntax tree, and then    */",
    "/* pretty-print the result.                                                 */",
    "/*                                                                          */",
    "/****************************************************************************/",
    "",
    "import " ++ name ++ "_pkg::*;",
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
   dat = identCat $ normCat cat
   def = identCat cat

svpkg name =
  unlines
   [  ""
    , "`ifndef " ++ (map toUpper name) ++ "_SVIO_PKG_SVH"
    , "`define " ++ (map toUpper name) ++ "_SVIO_PKG_SVH"
    , ""
    , "package " ++ name ++ "_pkg;"
    , ""
    , "`include \"" ++ name ++ "/" ++ name ++ "Absyn.svh\""
    , "`include \"" ++ name ++ "/" ++ name ++ "Parser.svh\""
    , "`include \"" ++ name ++ "/" ++ name ++ "Interp.svh\""
    , "`include \"" ++ name ++ "/" ++ name ++ "Printer.svh\""
    , ""
    , "`include \"" ++ name ++ "/" ++ name ++ "Absyn.sv\""
    , "`include \"" ++ name ++ "/" ++ name ++ "Interp.sv\""
    , "`include \"" ++ name ++ "/" ++ name ++ "Printer.sv\""
    , ""
    , "endpackage"
    , ""
    , "`endif"
   ]

fusesoc name =
  unlines
   [   "name: \"lib:interp:" ++ name ++ "\""
    ,  "description: \"interp lib\""
    ,  "filesets:"
    ,  "  pkg:"
    ,  "    files:"
    ,  ""
    ,  "      - " ++ name ++ "Absyn.svh: {is_include_file: true}"
    ,  "      - " ++ name ++ "Interp.svh: {is_include_file: true}"
    ,  "      - " ++ name ++ "Lexer.svh: {is_include_file: true}"
    ,  "      - " ++ name ++ "Printer.svh: {is_include_file: true}"
    ,  "      - bio.svh: {is_include_file: true}"
    ,  "      - " ++ name ++ "Parser.svh: {is_include_file: true}"
    ,  "      - " ++ name ++ "Absyn.sv: {is_include_file: true}"
    ,  "      - " ++ name ++ "Interp.sv: {is_include_file: true}"
    ,  "      - " ++ name ++ "Printer.sv: {is_include_file: true}"
    ,  ""
    ,  "      - " ++ name ++ "_pkg.sv"
    ,  "    file_type: systemVerilogSource"
    ,  ""
    ,  "  tb:"
    ,  "    files:"
    ,  "      - ../Test.sv"
    ,  "    file_type: systemVerilogSource"
    ,  ""
    ,  "targets:"
    ,  "  default: &default"
    ,  "    filesets:"
    ,  "      - pkg"
    ,  ""
    ,  "  sim: &sim"
    ,  "    <<: *default"
    ,  "    description: Simulate the design"
    ,  "    default_tool: modelsim"
    ,  "    filesets_append:"
    ,  "      - tb"
    ,  "    tools:"
    ,  "      vcs:"
    ,  "        vcs_options:"
    ,  "          - -g2012"
    ,  "      xsim:"
    ,  "        xelab_options:"
    ,  "          - \"--debug all\""
    ,  "      modelsim:"
    ,  "        vlog_options:"
    ,  "          - \"--debug all\""
    ,  ""
    ,  "  test:"
    ,  "    <<: *sim"
    ,  "    toplevel: tb_top"
    ,  "    default_tool: vcs"
    ,  "    parameters:"
    ,  "      - input=test.w"
    ,  ""
    ,  "parameters:"
    ,  "  input:"
    ,  "    datatype: str"
    ,  "    paramtype: plusarg"
    ,  ""
   ]
