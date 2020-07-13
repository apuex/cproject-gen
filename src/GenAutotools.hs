{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module GenAutotools (gen) where

import           System.IO                (stderr)
import           System.Exit
import           Control.Monad            (when)
import           Data.List
import           Data.Maybe               (fromMaybe)
import           Text.Printf
import qualified Data.Map              as M
import           Prelude               hiding (readFile, writeFile)
import           Text.Hamlet.XML
import           Text.XML
import           Text.Shakespeare.Text
import qualified Data.Text.Lazy        as TL
import qualified Data.Text.Lazy.IO     as TLIO
import           Data.Text                (Text)
import           Control.Monad            (forM_)
import qualified CmdLine               as CL


gen :: CL.Options -> String -> IO ()
gen opts file = do
    printf "%s: %s\n" (CL.paddingRight ("GenAutotools"::String) 12) file
    Document prologue root epilogue <- readFile def file
    fromElement opts root

fromElement :: CL.Options -> Element -> IO ()
fromElement opts root = do
    case root of
        Element "model" attrs children -> do
            mapM_ (\e -> printf "%s -> %s\n" (nameLocalName (fst e)) (snd e)) (M.toList attrs)
            TLIO.putStrLn $ genAutotools opts attrs children
        Element name attrs children -> do
            hPrintf stderr "Invalid root element name: %s\n" (show name)
            exitFailure
genAutotools :: CL.Options -> M.Map Name Text -> [Node] -> TL.Text
genAutotools opts attrs children = [lt|

|]
