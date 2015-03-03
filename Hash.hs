module Hash where

import Parsing.HashParser
import Language.Exec
import Language.Expressions
import Text.ParserCombinators.Parsec
import System.Directory
import Script

runScript :: FilePath -> IO ()
runScript f = Script.main

runInteractive :: IO ()
runInteractive = do
   cwd <- getCurrentDirectory 
   run ScriptState {wd = cwd, vartable = []}

run :: ScriptState -> IO ()
run state = do
   newComm <- getLine
   if newComm == "quit" then return () else do
      newState <- exec (parseInput newComm) state
      run newState


