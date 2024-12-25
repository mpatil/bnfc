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
    mkfile (name ++ "/" ++ uname ++ "Absyn.svh") commentWithEmacsModeHint hfile
    mkfile (name ++ "/" ++ uname ++ "Absyn.sv") commentWithEmacsModeHint cfile
    let (flex, env) = cf2lex (inPackage opts) name cf
    mkfile (name ++ ".l") commentWithEmacsModeHint flex
    let bison = cf2Yacc (linenumbers opts) (inPackage opts) name cf env
    mkfile (name ++ ".y") commentWithEmacsModeHint bison
    let (skelH, skelC) = cf2Interp (inPackage opts) name cf
    mkfile (name ++ "/" ++ uname ++ "Interp.svh") commentWithEmacsModeHint skelH
    mkfile (name ++ "/" ++ uname ++ "Interp.sv") commentWithEmacsModeHint skelC
    let (prinH, prinC) = cf2SVPrinter True (inPackage opts) name cf
    mkfile (name ++ "/" ++ uname ++ "Printer.svh") commentWithEmacsModeHint prinH
    mkfile (name ++ "/" ++ uname ++ "Printer.sv") commentWithEmacsModeHint prinC
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
    uname = map toUpper name

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
    , "`include \"" ++ name ++ "/" ++ uname ++ "Absyn.svh\""
    , "`include \"" ++ name ++ "/" ++ uname ++ "Parser.svh\""
    , "`include \"" ++ name ++ "/" ++ uname ++ "Interp.svh\""
    , "`include \"" ++ name ++ "/" ++ uname ++ "Printer.svh\""
    , ""
    , "`include \"" ++ name ++ "/" ++ uname ++ "Absyn.sv\""
    , "`include \"" ++ name ++ "/" ++ uname ++ "Interp.sv\""
    , "`include \"" ++ name ++ "/" ++ uname ++ "Printer.sv\""
    , ""
    , "endpackage"
    , ""
    , "`endif"
   ]
  where
    uname = map toUpper name

fusesoc name =
  unlines
   [   "name: \"lib:interp:" ++ name ++ "\""
    ,  "description: \"interp lib\""
    ,  "filesets:"
    ,  "  pkg:"
    ,  "    files:"
    ,  ""
    ,  "      - " ++ uname ++ "Absyn.svh: {is_include_file: true}"
    ,  "      - " ++ uname ++ "Interp.svh: {is_include_file: true}"
    ,  "      - " ++ uname ++ "Lexer.svh: {is_include_file: true}"
    ,  "      - " ++ uname ++ "Printer.svh: {is_include_file: true}"
    ,  "      - bio.svh: {is_include_file: true}"
    ,  "      - " ++ uname ++ "Parser.svh: {is_include_file: true}"
    ,  "      - " ++ uname ++ "Absyn.sv: {is_include_file: true}"
    ,  "      - " ++ uname ++ "Interp.sv: {is_include_file: true}"
    ,  "      - " ++ uname ++ "Printer.sv: {is_include_file: true}"
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
  where
    uname = map toUpper name
