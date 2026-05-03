module BNFC.Backend.SV.InterpSpec where

import Data.List (isInfixOf)

import System.Directory (setCurrentDirectory, doesFileExist)
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec
import Test.HUnit (assertFailure, assertBool)

import BNFC.CF (CF)
import BNFC.GetCF (parseCF)
import BNFC.Options (SharedOptions(..), Target(..), defaultOptions)
import BNFC.Backend.Base
  ( GeneratedFile(..)
  , execBackend
  , writeFiles
  )
import BNFC.Backend.SV (makeSV)
import BNFC.Backend.SV.Config
  ( mkSVConfig
  , svInterpBaseHeaderPath
  , svInterpBaseImplPath
  , svInterpHeaderPath
  , svInterpImplPath
  )

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

calcCfg :: SVConfigPaths
calcCfg = SVConfigPaths
  { baseH = svInterpBaseHeaderPath cfg
  , baseC = svInterpBaseImplPath   cfg
  , userH = svInterpHeaderPath     cfg
  , userC = svInterpImplPath       cfg
  }
  where cfg = mkSVConfig Nothing "Calc"

data SVConfigPaths = SVConfigPaths
  { baseH :: FilePath
  , baseC :: FilePath
  , userH :: FilePath
  , userC :: FilePath
  }

runBackend :: IO [GeneratedFile]
runBackend = do
  cf <- getCalc
  execBackend (makeSV calcOptions cf)

findFile :: FilePath -> [GeneratedFile] -> IO GeneratedFile
findFile path files = case filter ((== path) . fileName) files of
  [g] -> return g
  []  -> assertFailure ("file not generated: " ++ path)
           >> error "unreachable"
  gs  -> assertFailure ("file generated " ++ show (length gs)
                         ++ " times: " ++ path)
           >> error "unreachable"

shouldContain' :: String -> String -> Expectation
hay `shouldContain'` needle =
  assertBool ("expected substring " ++ show needle
               ++ " in:\n" ++ hay)
             (needle `isInfixOf` hay)

spec :: Spec
spec = describe "SV backend interpreter ownership" $ do

  describe "InterpBase header (regenerated)" $ do
    it "is emitted at <Lang>/<UPPER>InterpBase.svh" $ do
      _ <- findFile (baseH calcCfg) =<< runBackend
      return ()

    it "is overwritten on every run (writeOnce = False)" $ do
      g <- findFile (baseH calcCfg) =<< runBackend
      writeOnce g `shouldBe` False

    it "declares the abstract InterpBase visitor class" $ do
      g <- findFile (baseH calcCfg) =<< runBackend
      fileContent g `shouldContain'`
        "virtual class InterpBase implements Visitor;"

    it "declares an extern visit method for each user category" $ do
      g <- findFile (baseH calcCfg) =<< runBackend
      fileContent g `shouldContain'` "extern virtual task visitExp(Exp p);"

  describe "InterpBase impl (regenerated)" $ do
    it "is overwritten on every run (writeOnce = False)" $ do
      g <- findFile (baseC calcCfg) =<< runBackend
      writeOnce g `shouldBe` False

    it "defines abstract-class visit task bodies" $ do
      g <- findFile (baseC calcCfg) =<< runBackend
      fileContent g `shouldContain'` "task InterpBase::visitExp(Exp p)"

  describe "Interp header (write-once user file)" $ do
    it "is marked write-once (writeOnce = True)" $ do
      g <- findFile (userH calcCfg) =<< runBackend
      writeOnce g `shouldBe` True

    it "documents that semantic state belongs in the user header" $ do
      g <- findFile (userH calcCfg) =<< runBackend
      fileContent g `shouldContain'` "Add semantic state, helper classes, scope/environment logic,"
      fileContent g `shouldContain'` "Keep generic reusable runtime helpers in the generated package"

    it "declares Interp extending InterpBase" $ do
      g <- findFile (userH calcCfg) =<< runBackend
      fileContent g `shouldContain'` "class Interp extends InterpBase;"

    it "includes the InterpBase header" $ do
      g <- findFile (userH calcCfg) =<< runBackend
      fileContent g `shouldContain'` "InterpBase.svh"

  describe "Interp impl (write-once user file)" $ do
    it "is marked write-once (writeOnce = True)" $ do
      g <- findFile (userC calcCfg) =<< runBackend
      writeOnce g `shouldBe` True

    it "documents that simulator-specific semantic fixes stay user-owned" $ do
      g <- findFile (userC calcCfg) =<< runBackend
      fileContent g `shouldContain'` "Simulator-specific casts, delay expressions, and other"
      fileContent g `shouldContain'` "stay in this preserved file instead of generated scaffolding."

    it "delegates to super for each visit method" $ do
      g <- findFile (userC calcCfg) =<< runBackend
      fileContent g `shouldContain'` "super.visitExp"

    it "does not bake semantic-specific VCS casts into generated scaffolding" $ do
      g <- findFile (userC calcCfg) =<< runBackend
      fileContent g `shouldNotContain` "time'("
      fileContent g `shouldNotContain` ".itoa("
      fileContent g `shouldNotContain` ".hextoa("
      fileContent g `shouldNotContain` ".bintoa("

  describe "preservation semantics on disk" $ do
    it "preserves a pre-existing user-edited Interp.svh across regeneration" $
      withSystemTempDirectory "bnfc-sv-interp" $ \tmp -> do
        setCurrentDirectory tmp
        cf <- getCalc
        let userMarker = "// USER EDIT MARKER 17F3"
            userFile   = userH calcCfg
        -- First run: write all generated files (creates user file).
        writeFiles "." (makeSV calcOptions cf)
        doesFileExist userFile `shouldReturn` True
        -- Simulate a user edit.
        existing <- readFile userFile
        length existing `seq` writeFile userFile (existing ++ "\n" ++ userMarker ++ "\n")
        -- Second run must not clobber the user edit.
        writeFiles "." (makeSV calcOptions cf)
        after <- readFile userFile
        after `shouldContain'` userMarker

    it "regenerates InterpBase.svh on every run (no preservation)" $
      withSystemTempDirectory "bnfc-sv-interp" $ \tmp -> do
        setCurrentDirectory tmp
        cf <- getCalc
        let baseFile = baseH calcCfg
        writeFiles "." (makeSV calcOptions cf)
        writeFile baseFile "// stale content\n"
        writeFiles "." (makeSV calcOptions cf)
        regenerated <- readFile baseFile
        regenerated `shouldContain'` "virtual class InterpBase"
