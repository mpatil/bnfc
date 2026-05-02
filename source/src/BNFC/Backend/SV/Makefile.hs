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
    , "VCS = vcs"
    , "VCS_OPTS = -full64 -sverilog -l comp.log"
    , text $ "VCS_SIM = lib_interp_" ++ name ++ "_0"
    , "TOPLEVEL = test"
    , text $ "FUSESOC_RUN_DIR = lib_interp_" ++ name ++ "_0"
    , "TEST_VCS_DIR = ${FUSESOC_RUN_DIR}/test-vcs"
    , text $ "SIM_SCRIPT = lib_interp_" ++ name ++ "_0.scr"
    , "TEST_INPUT = ../../test.w"
    , ""
    , mkRule ".PHONY" ["all", "run", "qrun", "vcs", "clean", "distclean", "fusesoc"]
      []
    , mkRule "all" ["run"]
      []
    , mkRule "run" ["fusesoc", "Test.sv"]
      [ "@if command -v ${QRUN} >/dev/null 2>&1; then \\"
      , "  ${MAKE} qrun; \\"
      , "elif command -v ${VCS} >/dev/null 2>&1; then \\"
      , "  ${MAKE} vcs; \\"
      , "else \\"
      , "  echo \"Neither ${QRUN} nor ${VCS} found on PATH\"; \\"
      , "  exit 127; \\"
      , "fi"
      ]
    , mkRule "clean" []
      [ "rm -rf " ++ name ++ "/" ++ uname ++ "Parser.svh " ++ name ++ "/" ++ uname ++ "Lexer.svh qrun.out simv simv.daidir csrc ucli.key test-vcs ${FUSESOC_RUN_DIR}" ]
    , mkRule "distclean" ["clean"]
      [ "rm -f " ++ unwords
        [ name ++ "/" ++ uname ++ "Absyn.svh", name ++ "/" ++ uname ++ "Absyn.sv"
        , name ++ ".l", name ++ ".y"
        , name ++ "/" ++ uname ++ "Printer.sv", name ++ "/" ++ uname ++ "Printer.svh"
        , name ++ "/" ++ uname ++ "Interp.sv", name ++ "/" ++ uname ++ "Interp.svh"
        , "Test.sv", name ++ "/" ++ name ++ "_pkg.sv"
        , name ++ "/" ++ name ++ ".core", "comp.log", "vcs.log", "modelsim.ini"
        , name ++ "/*.bak", "y.output"
        , basename, name ++ ".tex"
        ]
      ]
    , ".PHONY: qrun"
    , mkRule "qrun" [ "fusesoc", "Test.sv" ]
      [ "cd ${TEST_VCS_DIR} && ${QRUN} ${QRUN_OPTS} -F ${SIM_SCRIPT} ../../../Test.sv +input=${TEST_INPUT}" ]
    , ".PHONY: vcs"
    , mkRule "vcs" [ "fusesoc", "Test.sv" ]
      [ "cd ${TEST_VCS_DIR} && ${VCS} ${VCS_OPTS} +incdir+../.. -f ${SIM_SCRIPT} -top ${TOPLEVEL} -o ${VCS_SIM}"
      , "cd ${TEST_VCS_DIR} && ./${VCS_SIM} -l vcs.log +input=${TEST_INPUT}"
      ]
    , mkRule (name ++ "/" ++ uname ++ "Lexer.svh") [ name ++ ".l" ]
      [ "${SVLEX} ${SVLEX_OPTS} " ++ name ++ ".l"
      , "mv lex.yy.v " ++ name ++ "/" ++ uname ++ "Lexer.svh"
      , "sed -i 's/`define yymore() (yymorfg=1)/`define yymore yymorfg=1/' " ++ name ++ "/" ++ uname ++ "Lexer.svh"
      ]
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
