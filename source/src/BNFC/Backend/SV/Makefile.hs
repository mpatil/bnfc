{-# LANGUAGE NoImplicitPrelude #-}

module BNFC.Backend.SV.Makefile (makefile) where

import Prelude hiding ((<>))

import BNFC.Backend.Common.Makefile
import BNFC.Backend.SV.Config
import BNFC.Backend.SV.Makefile.Simulator
import BNFC.PrettyPrint

makefile :: SVConfig -> String -> String -> Doc
makefile cfg _ basename = vcat $
    [
      "SVLEX = ${LEXPATH}/lex"
    , "SVLEX_OPTS = -v"
    , ""
    , "SVACC = ${YACCPATH}/svacc"
    , "SVACC_OPTS = -SVsemantic=svtype -v"
    , ""
    ] ++ simulatorVariables cfg ++
    [ ""
    , mkRule ".PHONY" ["all", "run", "qrun", "vcs", "clean", "distclean", "fusesoc"]
      []
    , mkRule "all" ["run"]
      []
    , mkRule "clean" []
      [ "rm -rf " ++ svDir cfg ++ "/" ++ svUpperName cfg ++ "Parser.svh " ++ svDir cfg ++ "/" ++ svUpperName cfg ++ "Lexer.svh qrun.out simv simv.daidir csrc ucli.key test-vcs ${FUSESOC_RUN_DIR}" ]
    , mkRule "distclean" ["clean"]
      [ "rm -f " ++ unwords
        [ svAbsynHeaderPath cfg, svAbsynImplPath cfg
        , svLexerPath cfg, svParserPath cfg
        , svPrinterImplPath cfg, svPrinterHeaderPath cfg
        , svInterpImplPath cfg, svInterpHeaderPath cfg
        , "Test.sv", svPackagePath cfg
        , svCorePath cfg, "comp.log", "vcs.log", "modelsim.ini"
        , svDir cfg ++ "/*.bak", "y.output"
        , basename, name ++ ".tex"
        ]
      ]
    , mkRule (svDir cfg ++ "/" ++ svUpperName cfg ++ "Lexer.svh") [ svLexerPath cfg ]
      [ "${SVLEX} ${SVLEX_OPTS} " ++ svLexerPath cfg
      , "mv lex.yy.v " ++ svDir cfg ++ "/" ++ svUpperName cfg ++ "Lexer.svh"
      , "sed -i 's/`define yymore() (yymorfg=1)/`define yymore yymorfg=1/' " ++ svDir cfg ++ "/" ++ svUpperName cfg ++ "Lexer.svh"
      ]
    , mkRule (svDir cfg ++ "/" ++ svUpperName cfg ++ "Parser.svh") [ svParserPath cfg ]
      [ "${SVACC} ${SVACC_OPTS} " ++ svParserPath cfg
      , "mv Parser.svh " ++ svDir cfg ++ "/" ++ svUpperName cfg ++ "Parser.svh "
      , "cp ${YACCPATH}/bio.svh " ++ svDir cfg ]
    , ".PHONY: fusesoc"
    , mkRule "fusesoc" [svDir cfg ++ "/" ++ svUpperName cfg ++ "Parser.svh", svDir cfg ++ "/" ++ svUpperName cfg ++ "Lexer.svh" ]
      [ "fusesoc --cores-root " ++ svDir cfg ++ " run --no-export --target=test --build-root=. --setup lib:interp:" ++ svName cfg ]
    ] ++ simulatorTargets cfg
  where
   name = svName cfg
