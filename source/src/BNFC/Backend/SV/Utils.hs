module BNFC.Backend.SV.Utils where

import Data.Char
import Data.Maybe (fromMaybe)

nsDefine :: Maybe String -> String -> String
nsDefine inPackage h = maybe h (\ns -> map toUpper ns ++ "_" ++ h) inPackage

nsStart :: Maybe String -> String
nsStart = maybe "" (\ns -> "namespace " ++ ns ++ "\n{")

nsEnd :: Maybe String -> String
nsEnd = maybe "" (const "}")

nsScope :: Maybe String -> String
nsScope = maybe "" (++ "::")

nsString :: Maybe String -> String
nsString = fromMaybe ""

svbasetypes = [
    ("Integer","int"),
    ("Char",   "string"),
    ("Double", "real"),
    ("String", "string"),
    ("Ident",  "string")
    ]
