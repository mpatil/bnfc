module BNFC.Backend.SV.SnapshotSpec where

import Data.List (isInfixOf, sort)

import Test.HUnit (assertBool, assertFailure)
import Test.Hspec

import BNFC.CF (CF)
import BNFC.GetCF (parseCF)
import BNFC.Options (SharedOptions(..), Target(..), defaultOptions)
import BNFC.Backend.Base (GeneratedFile(..), execBackend)
import BNFC.Backend.SV (makeSV)
import BNFC.Backend.SV.Config
  ( mkSVConfig
  , svAbsynHeaderPath
  , svInterpBaseHeaderPath
  , svInterpBaseImplPath
  , svInterpHeaderPath
  , svLexerPath
  , svParserPath
  , svPackagePath
  , svCorePath
  )

calcOptions :: SharedOptions
calcOptions = defaultOptions
  { lang = "Calc"
  , target = TargetSV
  , optMake = Just "Makefile"
  }

getCalc :: IO CF
getCalc = parseCF calcOptions TargetSV $ unlines
  [ "EAdd. Exp  ::= Exp \"+\" Exp1 ;"
  , "ESub. Exp  ::= Exp \"-\" Exp1 ;"
  , "EMul. Exp1 ::= Exp1 \"*\" Exp2 ;"
  , "EDiv. Exp1 ::= Exp1 \"/\" Exp2 ;"
  , "EInt. Exp2 ::= Integer ;"
  , "coercions Exp 2 ;"
  ]

cfg = mkSVConfig Nothing "Calc"

runBackend :: IO [GeneratedFile]
runBackend = do
  cf <- getCalc
  execBackend (makeSV calcOptions cf)

findContent :: FilePath -> [GeneratedFile] -> IO String
findContent path files = case filter ((== path) . fileName) files of
  [g] -> return (fileContent g)
  []  -> assertFailure ("file not generated: " ++ path)
           >> error "unreachable"
  gs  -> assertFailure ("file generated " ++ show (length gs)
                        ++ " times: " ++ path)
           >> error "unreachable"

shouldContain' :: String -> String -> Expectation
hay `shouldContain'` needle =
  assertBool ("expected substring " ++ show needle ++ " in:\n" ++ hay)
             (needle `isInfixOf` hay)

spec :: Spec
spec = describe "SV backend end-to-end snapshot" $ do

  it "emits the expected top-level SV artifact set for a small grammar" $ do
    files <- runBackend
    let names = sort (map fileName files)
        expected =
          sort
            [ svAbsynHeaderPath cfg
            , svInterpBaseHeaderPath cfg
            , svInterpBaseImplPath cfg
            , svInterpHeaderPath cfg
            , svLexerPath cfg
            , svParserPath cfg
            , svPackagePath cfg
            , svCorePath cfg
            , "Makefile"
            , "Test.sv"
            ]
    expected `shouldSatisfy` all (`elem` names)

  it "keeps key cross-file invariants aligned" $ do
    files <- runBackend
    absynH <- findContent (svAbsynHeaderPath cfg) files
    interpBaseH <- findContent (svInterpBaseHeaderPath cfg) files
    interpBaseC <- findContent (svInterpBaseImplPath cfg) files
    interpH <- findContent (svInterpHeaderPath cfg) files
    pkg <- findContent (svPackagePath cfg) files
    core <- findContent (svCorePath cfg) files
    mk <- findContent "Makefile" files
    testSv <- findContent "Test.sv" files
    parser <- findContent (svParserPath cfg) files

    absynH `shouldContain'` "typedef interface class Visitor;"
    parser `shouldContain'` "`ifndef CALC_ABSYN_HEADER"
    interpBaseH `shouldContain'` "virtual class InterpBase implements Visitor;"
    interpBaseC `shouldContain'` "task InterpBase::visitExp(Exp p)"
    interpH `shouldContain'` "class Interp extends InterpBase;"
    pkg `shouldContain'` "function automatic string str_replace"
    core `shouldContain'` "toplevel: test"
    mk `shouldContain'` "FUSESOC_RUN_DIR = lib_interp_Calc_0"
    testSv `shouldContain'` "program automatic test;"
