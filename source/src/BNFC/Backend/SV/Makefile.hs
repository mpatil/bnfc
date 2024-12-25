{-# LANGUAGE NoImplicitPrelude #-}

module BNFC.Backend.SV.Makefile (makefile) where

import Prelude hiding ((<>))
import Data.Char(toUpper)

import BNFC.Backend.Common.Makefile
import BNFC.PrettyPrint

makefile :: String -> String -> String -> Doc
makefile name _ basename = vcat
    [
      "SVLEX = ${LEXPATH}/lex"
    , "SVLEX_OPTS = -v"
    , ""
    , "SVACC = ${YACCPATH}/svacc"
    , "SVACC_OPTS = -SVsemantic=svtype -v"
    , ""
    , "QRUN = qrun"
    , "QRUN_OPTS = -64 -sv -mfcu -permissive -l comp.log -suppress 2875,2240 +incdir+./ +UVM_NO_RELNOTES"
    , ""
    , mkRule ".PHONY" ["clean", "distclean"]
      []
    , mkRule "all" ["qrun"]
      []
    , mkRule "clean" []
      [ "rm -rf " ++ name ++ "/" ++ uname ++ "Parser.svh " ++ name ++ "/" ++ uname ++ "Lexer.svh qrun.out test-vcs" ]
    , mkRule "distclean" ["clean"]
      [ "rm -f " ++ unwords
        [ name ++ "/" ++ uname ++ "Absyn.svh", name ++ "/" ++ uname ++ "Absyn.sv"
        , name ++ ".l", name ++ ".y"
        , name ++ "/" ++ uname ++ "Printer.sv", name ++ "/" ++ uname ++ "Printer.svh"
        , name ++ "/" ++ uname ++ "Interp.sv", name ++ "/" ++ uname ++ "Interp.svh"
        , "Test.sv", name ++ "/" ++ name ++ "_pkg.sv"
        , name ++ "/" ++ name ++ ".core", "comp.log", "modelsim.ini"
        , name ++ "/*.bak", "y.output"
        , basename, name ++ ".tex"
        ]
      ]
    , ".PHONY: qrun"
    , mkRule "qrun" [ "fusesoc", "Test.sv" ]
      [ "${QRUN} ${QRUN_OPTS} " ++ "-F test-vcs/lib_interp_" ++ name ++ "_0.scr Test.sv +input=test.w" ]
    , mkRule (name ++ "/" ++ uname ++ "Lexer.svh") [ name ++ ".l" ]
      [ "${SVLEX} ${SVLEX_OPTS} " ++ name ++ ".l"
      , "mv lex.yy.v " ++ name ++ "/" ++ uname ++ "Lexer.svh" ]
    , mkRule (name ++ "/" ++ uname ++ "Parser.svh") [ name ++ ".y" ]
      [ "${SVACC} ${SVACC_OPTS} " ++ name ++ ".y"
      , "mv Parser.svh " ++ name ++ "/" ++ uname ++ "Parser.svh "
      , "cp ${YACCPATH}/bio.svh " ++ name ]
    , ".PHONY: fusesoc"
    , mkRule "fusesoc" [name ++ "/" ++ uname ++ "Parser.svh", name ++ "/" ++ uname ++ "Lexer.svh" ]
      [ "fusesoc --cores-root " ++ name ++ " run --no-export --target=test --build-root=. --setup lib:interp:" ++ name ]
    ]
  where
   uname  = map toUpper name
