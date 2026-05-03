module BNFC.Backend.SV.Config
  ( SVConfig
  , mkSVConfig
  , svName
  , svUpperName
  , svNamespace
  , svDir
  , svAbsynHeaderPath
  , svAbsynImplPath
  , svInterpHeaderPath
  , svInterpImplPath
  , svInterpBaseHeaderPath
  , svInterpBaseImplPath
  , svPrinterHeaderPath
  , svPrinterImplPath
  , svPackagePath
  , svCorePath
  , svLexerPath
  , svParserPath
  , svPkgGuard
  , svAbsynSvGuard
  , svFuseSoCRunDir
  , svFuseSoCSimScript
  ) where

import Data.Char (toUpper)

data SVConfig = SVConfig
  { svName :: String
  , svUpperName :: String
  , svNamespace :: Maybe String
  }

mkSVConfig :: Maybe String -> String -> SVConfig
mkSVConfig ns name = SVConfig
  { svName = name
  , svUpperName = map toUpper name
  , svNamespace = ns
  }

svDir :: SVConfig -> FilePath
svDir cfg = svName cfg

svAbsynHeaderPath, svAbsynImplPath, svInterpHeaderPath, svInterpImplPath, svInterpBaseHeaderPath, svInterpBaseImplPath :: SVConfig -> FilePath
svPrinterHeaderPath, svPrinterImplPath, svPackagePath, svCorePath :: SVConfig -> FilePath
svLexerPath, svParserPath :: SVConfig -> FilePath

svAbsynHeaderPath cfg = svDir cfg ++ "/" ++ svUpperName cfg ++ "Absyn.svh"
svAbsynImplPath   cfg = svDir cfg ++ "/" ++ svUpperName cfg ++ "Absyn.sv"
svInterpHeaderPath cfg = svDir cfg ++ "/" ++ svUpperName cfg ++ "Interp.svh"
svInterpImplPath   cfg = svDir cfg ++ "/" ++ svUpperName cfg ++ "Interp.sv"
svInterpBaseHeaderPath cfg = svDir cfg ++ "/" ++ svUpperName cfg ++ "InterpBase.svh"
svInterpBaseImplPath   cfg = svDir cfg ++ "/" ++ svUpperName cfg ++ "InterpBase.sv"
svPrinterHeaderPath cfg = svDir cfg ++ "/" ++ svUpperName cfg ++ "Printer.svh"
svPrinterImplPath   cfg = svDir cfg ++ "/" ++ svUpperName cfg ++ "Printer.sv"
svPackagePath cfg = svDir cfg ++ "/" ++ svName cfg ++ "_pkg.sv"
svCorePath cfg = svDir cfg ++ "/" ++ svName cfg ++ ".core"
svLexerPath cfg = svName cfg ++ ".l"
svParserPath cfg = svName cfg ++ ".y"

svPkgGuard :: SVConfig -> String
svPkgGuard cfg = svUpperName cfg ++ "_SVIO_PKG_SVH"

svAbsynSvGuard :: SVConfig -> String
svAbsynSvGuard cfg = svUpperName cfg ++ "_ABSYN_SV"

svFuseSoCRunDir :: SVConfig -> String
svFuseSoCRunDir cfg = "lib_interp_" ++ svName cfg ++ "_0"

svFuseSoCSimScript :: SVConfig -> String
svFuseSoCSimScript cfg = svFuseSoCRunDir cfg ++ ".scr"
