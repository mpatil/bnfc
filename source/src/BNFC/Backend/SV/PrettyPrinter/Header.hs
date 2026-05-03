module BNFC.Backend.SV.PrettyPrinter.Header (mkHFile) where

import BNFC.Backend.SV.Config
import BNFC.Backend.SV.PrettyPrinter.Visitors
import BNFC.Backend.SV.Utils
import BNFC.CF

mkHFile :: Maybe String -> String -> CF -> [(Cat, [Rule])] -> String
mkHFile inPackage name cf groups = unlines
  [ printHeader
  , contentPrint
  , classFooter
  , showHeader
  , contentShow
  , classFooter
  , footer
  ]
  where
  printHeader = unlines
   [
    "`ifndef " ++ svUpperName cfg ++ "_" ++ hdef,
    "`define " ++ svUpperName cfg ++ "_" ++ hdef,
    "",
    "`include \"" ++ svAbsynHeaderPath cfg ++ "\"",
    nsStart inPackage,
    "/* Certain applications may improve performance by changing the buffer size */",
    "`define " ++ nsDefine inPackage "BUFFER_INITIAL" ++ " 2000",
    "/* You may wish to change _L_PAREN or _R_PAREN */",
    "`define " ++ nsDefine inPackage "_L_PAREN" ++ " \"(\"",
    "`define " ++ nsDefine inPackage "_R_PAREN" ++ " \")\"",
    "",
    "class PrintAbsyn implements Visitor;",
    " ",
    "  int _n_, _i_;",
    "  /* The following are simple heuristics for rendering terminals */",
    "  /* You may wish to change them */",
    "  extern task render(string c);",
    "  extern task render_s(string s);",
    "  extern task indent();",
    "  extern task backup();",
    " ",
    "  extern function new ();",
    " ",
    "  extern task print(Visitable v);"
   ]
  hdef = nsDefine inPackage "PRINTER_HEADER"
  cfg = mkSVConfig inPackage name
  contentShow = concatMap (prDataH False) groups
  contentPrint = concatMap (prDataH True) groups
  classFooter = unlines $
   [
    "  extern virtual task visitInteger(Integer x);",
    "  extern virtual task visitDouble(Double x);",
    "  extern virtual task visitChar(Char x);",
    "  extern virtual task visitString(string x);",
    "  extern virtual task visitIdent(string x);"
   ] ++ ["  extern virtual task visit" ++ t ++ "(" ++ t ++ " x);" | t <- tokenNames cf] ++
   [
    " ",
    "  string buf_;",
    "",
    "  task bufAppend(string s);",
    " ",
    "    buf_ = {buf_, s};",
    "  endtask",
    "",
    "  task bufReset();",
    " ",
    "    buf_ = \"\";",
    "  endtask",
    "",
    "endclass"
   ]
  showHeader = unlines
   [
    "",
    "class ShowAbsyn implements Visitor;",
    " ",
    "  extern function new();",
    "  extern task show(Visitable v);"
   ]
  footer = unlines
   [
    nsEnd inPackage,
    "`endif"
   ]
