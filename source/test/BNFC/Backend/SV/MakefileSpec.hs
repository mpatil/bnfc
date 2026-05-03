module BNFC.Backend.SV.MakefileSpec where

import Data.List (isInfixOf)

import Test.Hspec
import Test.HUnit (assertBool, assertFailure)

import BNFC.CF (CF)
import BNFC.GetCF (parseCF)
import BNFC.Options (SharedOptions(..), Target(..), defaultOptions)
import BNFC.Backend.Base (GeneratedFile(..), execBackend)
import BNFC.Backend.SV (makeSV)

calcOptions :: SharedOptions
calcOptions = defaultOptions { lang = "Calc", target = TargetSV }

getCalc :: IO CF
getCalc = parseCF calcOptions TargetSV $ unlines
  [ "EAdd. Exp  ::= Exp \"+\" Exp1 ;"
  , "ESub. Exp  ::= Exp \"-\" Exp1 ;"
  , "EMul. Exp1 ::= Exp1 \"*\" Exp2 ;"
  , "EDiv. Exp1 ::= Exp1 \"/\" Exp2 ;"
  , "EInt. Exp2 ::= Integer ;"
  , "coercions Exp 2 ;"
  ]

withMakefile :: String -> IO String
withMakefile name = do
  cf    <- getCalc
  let opts = calcOptions { optMake = Just name }
  files <- execBackend (makeSV opts cf)
  case filter ((== name) . fileName) files of
    [g] -> return (fileContent g)
    []  -> assertFailure ("Makefile not generated under name: " ++ name)
             >> error "unreachable"
    gs  -> assertFailure ("Makefile generated " ++ show (length gs)
                           ++ " times: " ++ name)
             >> error "unreachable"

shouldContain' :: String -> String -> Expectation
hay `shouldContain'` needle =
  assertBool ("expected substring " ++ show needle
              ++ " in:\n" ++ hay)
             (needle `isInfixOf` hay)

spec :: Spec
spec = describe "SV backend Makefile" $ do

  it "is omitted when --makefile is not set" $ do
    cf    <- getCalc
    files <- execBackend (makeSV calcOptions cf)
    -- No file named "Makefile" without optMake.
    let names = map fileName files
    assertBool ("unexpected Makefile in " ++ show names)
               (notElem "Makefile" names)

  it "honours the --makefile=<name> option" $ do
    _ <- withMakefile "MyMakefile"
    return ()

  it "declares SVLEX and SVACC build variables" $ do
    mk <- withMakefile "Makefile"
    mk `shouldContain'` "SVLEX = ${LEXPATH}/lex"
    mk `shouldContain'` "SVACC = ${YACCPATH}/svacc"

  it "declares simulator variables for QRUN, VCS and the FuseSoC run dir" $ do
    mk <- withMakefile "Makefile"
    mk `shouldContain'` "QRUN = qrun"
    mk `shouldContain'` "VCS = vcs"
    mk `shouldContain'` "FUSESOC_RUN_DIR = lib_interp_Calc_0"
    mk `shouldContain'` "SIM_SCRIPT = lib_interp_Calc_0.scr"

  it "declares all/run/qrun/vcs/clean/distclean/fusesoc as .PHONY" $ do
    mk <- withMakefile "Makefile"
    mk `shouldContain'` ".PHONY"
    mk `shouldContain'` "all run qrun vcs clean distclean fusesoc"

  it "provides a fusesoc target invoking the lib:interp:<lang> core" $ do
    mk <- withMakefile "Makefile"
    mk `shouldContain'` "fusesoc --cores-root Calc"
    mk `shouldContain'` "lib:interp:Calc"

  it "provides a run target that auto-selects qrun or vcs" $ do
    mk <- withMakefile "Makefile"
    mk `shouldContain'` "command -v ${QRUN}"
    mk `shouldContain'` "command -v ${VCS}"

  it "provides qrun and vcs targets that cd into TEST_VCS_DIR" $ do
    mk <- withMakefile "Makefile"
    mk `shouldContain'` "cd ${TEST_VCS_DIR} && ${QRUN}"
    mk `shouldContain'` "cd ${TEST_VCS_DIR} && ${VCS}"

  it "does not post-process the lexer header (sv-lex emits VCS-safe macros)" $ do
    mk <- withMakefile "Makefile"
    -- The yymore sed workaround was lifted into sv-lex/src/header.c.
    -- BNFC must not reintroduce post-emit repair of CALCLexer.svh.
    assertBool ("Makefile reintroduced sed workaround:\n" ++ mk)
               (not ("sed -i" `isInfixOf` mk))
    assertBool ("Makefile reintroduced yymore repair:\n" ++ mk)
               (not ("yymore" `isInfixOf` mk))

  it "lists InterpBase artifacts in distclean (regenerated, safe to delete)" $ do
    mk <- withMakefile "Makefile"
    mk `shouldContain'` "Calc/CALCInterpBase.svh"
    mk `shouldContain'` "Calc/CALCInterpBase.sv"

  it "does not list user-owned Interp.svh / Interp.sv in distclean" $ do
    mk <- withMakefile "Makefile"
    -- Distclean lists files with a trailing space (unwords-separated).
    -- InterpBase paths are valid; user-owned Interp paths must not appear.
    let userH = "Calc/CALCInterp.svh "
        userC = "Calc/CALCInterp.sv "
    assertBool ("distclean must not delete " ++ userH)
               (not (userH `isInfixOf` mk))
    assertBool ("distclean must not delete " ++ userC)
               (not (userC `isInfixOf` mk))
