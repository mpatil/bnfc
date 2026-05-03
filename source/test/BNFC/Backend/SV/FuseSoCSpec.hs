module BNFC.Backend.SV.FuseSoCSpec where

import Data.List (isInfixOf)

import Test.Hspec
import Test.HUnit (assertBool, assertFailure)

import BNFC.CF (CF)
import BNFC.GetCF (parseCF)
import BNFC.Options (SharedOptions(..), Target(..), defaultOptions)
import BNFC.Backend.Base (GeneratedFile(..), execBackend)
import BNFC.Backend.SV (makeSV)
import BNFC.Backend.SV.Config (mkSVConfig, svCorePath)

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

corePath :: FilePath
corePath = svCorePath (mkSVConfig Nothing "Calc")

getCore :: IO String
getCore = do
  cf    <- getCalc
  files <- execBackend (makeSV calcOptions cf)
  case filter ((== corePath) . fileName) files of
    [g] -> return (fileContent g)
    []  -> assertFailure ("file not generated: " ++ corePath)
             >> error "unreachable"
    gs  -> assertFailure ("file generated " ++ show (length gs)
                          ++ " times: " ++ corePath)
             >> error "unreachable"

shouldContain' :: String -> String -> Expectation
hay `shouldContain'` needle =
  assertBool ("expected substring " ++ show needle
              ++ " in:\n" ++ hay)
             (needle `isInfixOf` hay)

spec :: Spec
spec = describe "SV backend FuseSoC core file" $ do

  it "is emitted at <Lang>/<lang>.core" $ do
    _ <- getCore
    return ()

  it "declares the lib:interp:<lang> core name" $ do
    core <- getCore
    core `shouldContain'` "name: \"lib:interp:Calc\""

  it "lists Absyn/Interp/InterpBase headers in the pkg fileset" $ do
    core <- getCore
    core `shouldContain'` "CALCAbsyn.svh: {is_include_file: true}"
    core `shouldContain'` "CALCInterpBase.svh: {is_include_file: true}"
    core `shouldContain'` "CALCInterp.svh: {is_include_file: true}"
    core `shouldContain'` "CALCParser.svh: {is_include_file: true}"
    core `shouldContain'` "CALCLexer.svh: {is_include_file: true}"
    core `shouldContain'` "CALCPrinter.svh: {is_include_file: true}"

  it "lists the language pkg.sv as a non-include source" $ do
    core <- getCore
    core `shouldContain'` "Calc_pkg.sv"

  it "declares the tb fileset pointing at Test.sv" $ do
    core <- getCore
    core `shouldContain'` "tb:"
    core `shouldContain'` "../Test.sv"

  it "defines default, sim, and test targets" $ do
    core <- getCore
    core `shouldContain'` "  default: &default"
    core `shouldContain'` "  sim: &sim"
    core `shouldContain'` "  test:"

  it "wires the test target to vcs by default" $ do
    core <- getCore
    core `shouldContain'` "default_tool: vcs"

  it "exposes the input plusarg parameter" $ do
    core <- getCore
    core `shouldContain'` "input:"
    core `shouldContain'` "paramtype: plusarg"
