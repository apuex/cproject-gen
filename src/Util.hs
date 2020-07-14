{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Util where

import qualified Data.Char           as C
import qualified Data.Text           as T


cToPascal :: T.Text -> T.Text
cToPascal str = T.concat $ map capitalize $ T.splitOn underscore $ T.map C.toLower str

cToCamel :: T.Text -> T.Text
cToCamel str = T.concat $ capRest $ T.splitOn underscore $ T.map C.toLower str
    where capRest    (s:xs) = s:(map capitalize xs)
          capRest        [] = []   

cToShell :: T.Text -> T.Text
cToShell str = T.map shell $ T.map C.toLower str
    where shell c = if c == '_' then '-' else c

cToConst :: T.Text -> T.Text
cToConst str = T.toUpper str

capitalize :: T.Text -> T.Text
capitalize str = if T.null str then str
                 else T.cons f t where f = C.toUpper $ T.head str
                                       t = T.tail str


underscore :: T.Text
underscore = "_"
