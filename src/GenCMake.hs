{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module GenCMake (gen) where

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
    printf "%s: %s\n" (CL.paddingRight ("GenCMake"::String) 12) file
    Document prologue root epilogue <- readFile def file
    fromElement opts root

fromElement :: CL.Options -> Element -> IO ()
fromElement opts root = do
    case root of
        Element "model" attrs children -> do
            mapM_ (\e -> printf "%s -> %s\n" (nameLocalName (fst e)) (snd e)) (M.toList attrs)
            TLIO.putStrLn $ genCMakeLists opts attrs children
            return ()
        Element name attrs children -> do
            hPrintf stderr "Invalid root element name: %s\n" (show name)
            exitFailure

genCMakeLists :: CL.Options -> M.Map Name Text -> [Node] -> TL.Text
genCMakeLists opts attrs children = [lt|
CMAKE_MINIMUM_REQUIRED(VERSION 3.0)
PROJECT(sample)

INCLUDE_DIRECTORIES(include
  ${INCLUDE_DIRECTORIES}
)

LINK_DIRECTORIES(
  ${LINK_DIRECTORIES}
  ${LIBRARY_OUTPUT_PATH}
)

INCLUDE_DIRECTORIES(
  include
)
|]

genTargets :: CL.Options -> [Node] -> [TL.Text]
genTargets opts nodes = concat $ map genTarget nodes
    where genTarget node = case node of
            (NodeElement e) -> target e
            (NodeContent _) -> []
            (NodeComment _) -> []
            (NodeInstruction _) -> []
          target e = case e of
            (Element "executable" attrs children) -> [genExecutable opts attrs children]
            (Element "library" attrs children) -> [genLibrary opts attrs children]
            _ -> []


genExecutable :: CL.Options -> M.Map Name Text -> [Node] -> TL.Text
genExecutable opts attrs children = [lt|
SET(my_exe_SRCS
)

ADD_EXECUTABLE(my-exe ${my_exe_SRCS})

INSTALL(TARGETS my-exe
    CONFIGURATIONS Release
    RUNTIME DESTINATION bin PERMISSIONS WORLD_EXECUTE
    LIBRARY DESTINATION lib PERMISSIONS WORLD_EXECUTE
    ARCHIVE DESTINATION lib)
INSTALL(TARGETS my-exe
    CONFIGURATIONS Debug
    RUNTIME DESTINATION bin PERMISSIONS WORLD_EXECUTE
    LIBRARY DESTINATION lib PERMISSIONS WORLD_EXECUTE
    ARCHIVE DESTINATION lib)
|]

genLibrary :: CL.Options -> M.Map Name Text -> [Node] -> TL.Text
genLibrary opts attrs children = [lt|
SET(my_lib_INCLUDES
)
SET(my_lib_SRCS
)

ADD_LIBRARY(my-lib ${my_lib_SRCS})

INSTALL(TARGETS my-lib
    CONFIGURATIONS Release
    RUNTIME DESTINATION bin PERMISSIONS WORLD_EXECUTE
    LIBRARY DESTINATION lib PERMISSIONS WORLD_EXECUTE
    ARCHIVE DESTINATION lib)
INSTALL(TARGETS my-lib
    CONFIGURATIONS Debug
    RUNTIME DESTINATION bin PERMISSIONS WORLD_EXECUTE
    LIBRARY DESTINATION lib PERMISSIONS WORLD_EXECUTE
    ARCHIVE DESTINATION lib)
|]


