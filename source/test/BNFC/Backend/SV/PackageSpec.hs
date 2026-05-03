module BNFC.Backend.SV.PackageSpec where

import Data.List (isInfixOf)

import Test.Hspec
import Test.HUnit (assertBool, assertFailure)

import BNFC.CF (CF)
import BNFC.GetCF (parseCF)
import BNFC.Options (SharedOptions(..), Target(..), defaultOptions)
import BNFC.Backend.Base (GeneratedFile(..), execBackend)
import BNFC.Backend.SV (makeSV)
import BNFC.Backend.SV.Config (mkSVConfig, svPackagePath)

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

pkgPath :: FilePath
pkgPath = svPackagePath (mkSVConfig Nothing "Calc")

getPackage :: IO String
getPackage = do
  cf    <- getCalc
  files <- execBackend (makeSV calcOptions cf)
  case filter ((== pkgPath) . fileName) files of
    [g] -> return (fileContent g)
    []  -> assertFailure ("file not generated: " ++ pkgPath)
             >> error "unreachable"
    gs  -> assertFailure ("file generated " ++ show (length gs)
                          ++ " times: " ++ pkgPath)
             >> error "unreachable"

shouldContain' :: String -> String -> Expectation
hay `shouldContain'` needle =
  assertBool ("expected substring " ++ show needle
              ++ " in:\n" ++ hay)
             (needle `isInfixOf` hay)

spec :: Spec
spec = describe "SV backend package file" $ do

  it "is emitted at <Lang>/<lang>_pkg.sv" $ do
    _ <- getPackage
    return ()

  it "includes the interpreter base and user headers" $ do
    pkg <- getPackage
    pkg `shouldContain'` "`include \"Calc/CALCInterpBase.svh\""
    pkg `shouldContain'` "`include \"Calc/CALCInterp.svh\""

  it "includes the interpreter base and user implementations" $ do
    pkg <- getPackage
    pkg `shouldContain'` "`include \"Calc/CALCInterpBase.sv\""
    pkg `shouldContain'` "`include \"Calc/CALCInterp.sv\""

  it "defines the generated string helper functions" $ do
    pkg <- getPackage
    pkg `shouldContain'` "function automatic int str_find"
    pkg `shouldContain'` "function automatic string str_replace"

  it "documents that only shared runtime helpers belong in the package" $ do
    pkg <- getPackage
    pkg `shouldContain'` "Generated shared runtime helpers."
    pkg `shouldContain'` "in the preserved Interp user files instead of this package support."

  it "does not embed interpreter-specific semantic classes in the package" $ do
    pkg <- getPackage
    pkg `shouldNotContain` "class Symbol;"
    pkg `shouldNotContain` "class Env;"
