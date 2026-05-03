{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module BNFC.Backend.BaseSpec where

import Data.List (isInfixOf)

import System.Directory
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import BNFC.Backend.Base -- SUT

default(String)

spec :: Spec
spec = do
  -- -- Andreas, 2021-07-17
  -- -- I don't really believe in these unit tests; important are system tests.
  -- -- So I am not putting in much energy to maintain them.
  describe "Backend monad" $ do
    it "empty computation generates empty list of files" $
      execBackend (return ()) `shouldReturn` []
    it "marks mkfileOnce outputs as write-once" $
      execBackend (mkfileOnce "test.txt" id "abcd") `shouldReturn`
        [GeneratedFile "test.txt" id True "abcd\n"]
    -- -- Test broken: mkfile also puts the BNFC signature containing the version number.
    -- it "returns the file created using mkfile" $
    --   execBackend (mkfile "test.txt" "abcd")
    --     `shouldReturn` [("test.txt", "abcd\n")]
  describe "writeFiles" $ do
    it "creates the root directory if it doesn't exists" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "foo/bar" (return ())
        doesDirectoryExist "foo/bar" `shouldReturn` True
    it "creates a file from the bucket" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "." (mkfile "file.txt" id "")
        doesFileExist "file.txt"
      `shouldReturn` True
    it "put the right content in the file" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "." (mkfile "file.txt" id "abcd")
        readFile "file.txt" >>=
          (`shouldSatisfy` isInfixOf "abcd\n")
    it "creates subdirectories" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "." (mkfile "subdir/file.txt" id "abcd")
        doesDirectoryExist "subdir"
      `shouldReturn` True
    it "creates files in the root directory" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "root/" (mkfile "foo/bar.txt" id "abcd")
        doesFileExist "root/foo/bar.txt" `shouldReturn` True
    it "creates a missing file for mkfileOnce" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "." (mkfileOnce "file.txt" id "fresh")
        readFile "file.txt" >>= (`shouldSatisfy` isInfixOf "fresh\n")
    it "preserves an existing file for mkfileOnce" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFile "file.txt" "user content\n"
        writeFiles "." (mkfileOnce "file.txt" id "generated")
        readFile "file.txt" `shouldReturn` "user content\n"
    it "still overwrites an existing file for mkfile" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFile "file.txt" "user content\n"
        writeFiles "." (mkfile "file.txt" id "generated")
        readFile "file.txt" >>= (`shouldSatisfy` isInfixOf "generated\n")
