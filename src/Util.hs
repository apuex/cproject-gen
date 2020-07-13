module Util where

import Data.Char
import Data.List.Split


cToPascal :: String -> String
cToPascal str = concat $ map capitalize $ splitOn "_" $ map toLower str

cToCamel :: String -> String
cToCamel str = concat $ capRest $ splitOn "_" $ map toLower str
    where capRest    (s:xs) = s:(map capitalize xs)
          capRest        [] = []   

capitalize :: String -> String
capitalize (s:xs) = (toUpper s) : xs
capitalize     [] = []
