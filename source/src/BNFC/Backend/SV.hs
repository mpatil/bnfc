{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.SV (makeSV,) where

import Prelude

import BNFC.CF
import BNFC.Options
import BNFC.Backend.Base
import qualified BNFC.Backend.Common.Makefile as Makefile

import BNFC.Backend.SV.Config
import BNFC.Backend.SV.CFtoAbs
import BNFC.Backend.SV.CFtoLex
import BNFC.Backend.SV.CFtoYacc
import BNFC.Backend.SV.CFtoInterp
import BNFC.Backend.SV.FuseSoC
import BNFC.Backend.SV.Makefile
import BNFC.Backend.SV.Package
import BNFC.Backend.SV.PrettyPrinter
import BNFC.Backend.SV.Testbench
import BNFC.Utils

makeSV :: SharedOptions -> CF -> MkFiles ()
makeSV opts cf = do
    let cfg = mkSVConfig (inPackage opts) name
    let (hfile, cfile) = cf2SVAbs (linenumbers opts) (inPackage opts) name cf
    mkfile (svAbsynHeaderPath cfg) commentWithEmacsModeHint hfile
    mkfile (svAbsynImplPath cfg) commentWithEmacsModeHint cfile
    let (flex, env) = cf2lex (inPackage opts) name cf
    mkfile (svLexerPath cfg) commentWithEmacsModeHint flex
    let bison = cf2Yacc (linenumbers opts) (inPackage opts) name cf env
    mkfile (svParserPath cfg) commentWithEmacsModeHint bison
    let (baseH, baseC, userH, userC) = cf2Interp (inPackage opts) name cf
    mkfile (svInterpBaseHeaderPath cfg) commentWithEmacsModeHint baseH
    mkfile (svInterpBaseImplPath cfg) commentWithEmacsModeHint baseC
    mkfileOnce (svInterpHeaderPath cfg) commentWithEmacsModeHint userH
    mkfileOnce (svInterpImplPath cfg) commentWithEmacsModeHint userC
    let (prinH, prinC) = cf2SVPrinter True (inPackage opts) name cf
    mkfile (svPrinterHeaderPath cfg) commentWithEmacsModeHint prinH
    mkfile (svPrinterImplPath cfg) commentWithEmacsModeHint prinC
    mkfile "Test.sv" commentWithEmacsModeHint (svtest cf cfg)
    Makefile.mkMakefile (optMake opts) $ makefile cfg prefix
    mkfile (svPackagePath cfg) commentWithEmacsModeHint (svpkg cfg)
    mkfile (svCorePath cfg) ("CAPI=2:\n# " ++) (fusesoc cfg)
  where
    name :: String
    name = lang opts
    -- The prefix is a string used by flex and bison
    -- that is prepended to generated function names.
    -- It should be a valid C identifier.
    prefix :: String
    prefix = snakeCase_ name ++ "_"

-- | Put string into a block comment.
comment :: String -> String
comment x = unwords ["/*", x, "*/"]

-- | C line comment including mode hint for emacs.
commentWithEmacsModeHint :: String -> String
commentWithEmacsModeHint = comment . ("-*- sv -*- " ++)
