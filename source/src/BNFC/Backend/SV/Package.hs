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
    , "`include \"" ++ svInterpBaseHeaderPath cfg ++ "\""
    , "`include \"" ++ svInterpHeaderPath cfg ++ "\""
    , "`include \"" ++ svPrinterHeaderPath cfg ++ "\""
    , ""
    , packageHelpers
    , ""
    , "`include \"" ++ svAbsynImplPath cfg ++ "\""
    , "`include \"" ++ svInterpBaseImplPath cfg ++ "\""
    , "`include \"" ++ svInterpImplPath cfg ++ "\""
    , "`include \"" ++ svPrinterImplPath cfg ++ "\""
    , ""
    , "endpackage"
    , ""
    , "`endif"
   ]

packageHelpers :: String
packageHelpers = unlines
  [ "  // Returns the index of first occurrence of string 'sub' within string 's''s given index range."
  , "  // Returns -1 otherwise."
  , "  function automatic int str_find(string s, string sub, int range_lo = 0, int range_hi = -1);"
  , "    if (range_hi < 0 || range_hi >= s.len()) range_hi = s.len() - 1;"
  , "    for (int i = range_lo; i <= (range_hi - sub.len() + 1); i++) begin"
  , "      if (s.substr(i, i + sub.len() - 1) == sub) begin"
  , "        return i;"
  , "      end"
  , "    end"
  , "    return -1;"
  , "  endfunction : str_find"
  , ""
  , "  // Find the first match string 'sub' in 's' and replace it with 'new_sub'."
  , "  // TODO: Add support for global replacement."
  , "  function automatic string str_replace(string s, string sub, string new_sub);"
  , "    string str_before_sub, str_after_sub;"
  , "    int lo_idx = str_find(s, sub);"
  , ""
  , "    if (!(lo_idx != -1))"
  , "      return s;"
  , ""
  , "    if (lo_idx > 0) str_before_sub = s.substr(0, lo_idx - 1);"
  , "    if (lo_idx + sub.len() < s.len()) str_after_sub = s.substr(lo_idx + sub.len(), s.len() - 1);"
  , ""
  , "    return {str_before_sub, new_sub, str_after_sub};"
  , "  endfunction : str_replace"
  ]
