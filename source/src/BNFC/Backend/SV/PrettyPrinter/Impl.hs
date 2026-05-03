module BNFC.Backend.SV.PrettyPrinter.Impl (mkCFile) where

import BNFC.Backend.SV.Config
import BNFC.Backend.SV.PrettyPrinter.Render
import BNFC.Backend.SV.PrettyPrinter.Visitors
import BNFC.Backend.SV.Utils
import BNFC.CF

mkCFile :: Maybe String -> String -> CF -> [(Cat, [Rule])] -> String
mkCFile inPackage name cf groups = concat
   [
    header,
    nsStart inPackage ++ "\n",
    prRender,
    printEntries,
    concatMap (prPrintData inPackage cf) groups,
    printBasics,
    printTokens,
    showEntries,
    concatMap prShowData groups,
    showBasics,
    showTokens,
    nsEnd inPackage ++ "\n"
   ]
  where
    header = unlines
     [
      "/*** Generated Pretty Printer and Abstract Syntax Viewer ***/",
      "",
      "`include \"" ++ svPrinterHeaderPath cfg ++ "\"",
      "`define INDENT_WIDTH 2",
      ""
     ]
    cfg = mkSVConfig inPackage name
    printEntries = unlines
     [
      "function PrintAbsyn::new();",
      "",
      "  _i_ = 0; _n_ = 0;",
      "  buf_ = \"\";",
      "endfunction",
      "",
      "task PrintAbsyn::print(Visitable v);",
      "",
      "  _i_ = 0; _n_ = 0;",
      "  buf_=\"\";",
      "  v.accept(this);",
      "endtask",
      ""
     ]
    showEntries = unlines
     [
      "function ShowAbsyn::new();",
      "  buf_ = \"\";",
      "endfunction",
      "",
      "",
      "task ShowAbsyn::show(Visitable v);",
      "",
      "  buf_=\"\";",
      "  v.accept(this);",
      "endtask",
      ""
     ]
    printBasics = unlines
     [
      "task PrintAbsyn::visitInteger(Integer x);",
      "",
      "  string tmp = $psprintf(\"%d\", x);",
      "  bufAppend(tmp);",
      "endtask",
      "",
      "task PrintAbsyn::visitDouble(Double x);",
      "",
      "  string tmp = $psprintf(\"%g\", x);",
      "  bufAppend(tmp);",
      "endtask",
      "",
      "task PrintAbsyn::visitChar(Char x);",
      "",
      "  bufAppend(\"\\\"\");",
      "  bufAppend(x);",
      "  bufAppend(\"\\\"\");",
      "endtask",
      "",
      "task PrintAbsyn::visitString(string x);",
      "",
      "  bufAppend(\"\\\"\");",
      "  bufAppend(x);",
      "  bufAppend(\"\\\"\");",
      "endtask",
      "",
      "task PrintAbsyn::visitIdent(string x);",
      "",
      "  render(x);",
      "endtask",
      ""
     ]
    printTokens = unlines
     [unlines
      [ "task PrintAbsyn::visit" ++ t ++ "(" ++ t ++ " x);"
      , ""
      , "  render(x);"
      , "endtask"
      , ""
      ]
     | t <- tokenNames cf
     ]
    showBasics = unlines
     [
      "task ShowAbsyn::visitInteger(Integer x);",
      "",
      "  string tmp = $psprintf(\"%d\", x);",
      "  bufAppend(tmp);",
      "endtask",
      "",
      "task ShowAbsyn::visitDouble(Double x);",
      "  string tmp = $psprintf(\"%g\", x);",
      "  bufAppend(tmp);",
      "endtask",
      "",
      "task ShowAbsyn::visitChar(Char x);",
      "  bufAppend(\"\\\"\");",
      "  bufAppend(x);",
      "  bufAppend(\"\\\"\");",
      "endtask",
      "",
      "task ShowAbsyn::visitString(string x);",
      "  bufAppend(\"\\\"\");",
      "  bufAppend(x);",
      "  bufAppend(\"\\\"\");",
      "endtask",
      "",
      "task ShowAbsyn::visitIdent(string x);",
      "  bufAppend(\"\\\"\");",
      "  bufAppend(x);",
      "  bufAppend(\"\\\"\");",
      "endtask",
      ""
     ]
    showTokens = unlines
     [unlines
      [ "task ShowAbsyn::visit" ++ t ++ "(" ++ t ++ " x);"
      , ""
      , "  bufAppend(\"\\\"\");"
      , "  bufAppend(x);"
      , "  bufAppend(\"\\\"\");"
      , "endtask"
      , ""
      ]
     | t <- tokenNames cf
     ]
