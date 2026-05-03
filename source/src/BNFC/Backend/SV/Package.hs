{-# LANGUAGE NoImplicitPrelude #-}

module BNFC.Backend.SV.Package (svpkg) where

import Prelude (String, (++), unlines)

import BNFC.Backend.SV.Config

svpkg :: SVConfig -> String
svpkg cfg =
  unlines
   [  ""
    , "`ifndef " ++ svPkgGuard cfg
    , "`define " ++ svPkgGuard cfg
    , ""
    , "package " ++ svName cfg ++ "_pkg;"
    , ""
    , "`include \"" ++ svAbsynHeaderPath cfg ++ "\""
    , "`include \"" ++ svDir cfg ++ "/" ++ svUpperName cfg ++ "Parser.svh\""
    , "`include \"" ++ svInterpHeaderPath cfg ++ "\""
    , "`include \"" ++ svPrinterHeaderPath cfg ++ "\""
    , ""
    , "`include \"" ++ svAbsynImplPath cfg ++ "\""
    , "`include \"" ++ svInterpImplPath cfg ++ "\""
    , "`include \"" ++ svPrinterImplPath cfg ++ "\""
    , ""
    , "endpackage"
    , ""
    , "`endif"
   ]
