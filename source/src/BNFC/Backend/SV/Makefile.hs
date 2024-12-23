{-# LANGUAGE NoImplicitPrelude #-}

module BNFC.Backend.SV.Makefile (makefile) where

import Prelude hiding ((<>))

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
      [ "rm -rf " ++ name ++ "Parser.svh " ++ name ++ "Lexer.svh qrun.out test-vcs" ]
    , mkRule "distclean" ["clean"]
      [ "rm -f " ++ unwords
        [ name ++ "/" ++ name ++ "Absyn.svh", name ++ "/" ++ name ++ "Absyn.sv"
        , name ++ ".l", name ++ ".y"
        , name ++ "/" ++ name ++ "Printer.sv", name ++ "/" ++ name ++ "Printer.svh"
        , name ++ "/" ++ name ++ "Interp.sv", name ++ "/" ++ name ++ "Interp.svh"
        , "Test.sv", name ++ "_pkg.sv"
        , name ++ ".core", "comp.log", "modelsim.ini"
        , name ++ "/*.bak", name ++ "/y.output"
        , basename, name ++ ".tex"
        ]
      ]
    , ".PHONY: qrun"
    , mkRule "qrun" [ "fusesoc", "Test.sv" ]
      [ "${QRUN} ${QRUN_OPTS} " ++ "-F test-vcs/lib_interp_" ++ name ++ "_0.scr Test.sv +input=test.w" ]
    , mkRule (name ++ "/" ++ name ++ "Lexer.svh") [ name ++ ".l" ]
      [ "${SVLEX} ${SVLEX_OPTS} " ++ name ++ ".l"
      , "mv lex.yy.v " ++ name ++ "/" ++ name ++ "Lexer.svh" ]
    , mkRule (name ++ "/" ++ name ++ "Parser.svh") [ name ++ ".y" ]
      [ "${SVACC} ${SVACC_OPTS} " ++ name ++ ".y"
      , "mv Parser.svh " ++ name ++ "/" ++ name ++ "Parser.svh "]
    , ".PHONY: fusesoc"
    , mkRule "fusesoc" [name ++ "/" ++ name ++ "Parser.svh", name ++ "/" ++ name ++ "Lexer.svh" ]
      [ "fusesoc --cores-root . run --no-export --target=test --build-root=. --setup lib:interp:" ++ name ]
    ]
