module Main ( main ) where

import Parser
import TypeInference
import Interpreter

import System.Environment ( getArgs )

printError :: String -> String -> IO ()
printError desc err = do
  putStrLn desc
  putStrLn err

main :: IO ()
main = do
  args     <- getArgs
  case args of 
    fileName:_ -> do
      code <- readFile fileName
      -- putStrLn code
      case parseProgram code of
        Left err   -> printError "Parse error:" $ show err
        Right prog ->
          case infereType prog of
            Left err -> printError "Type inference error:" err
            Right _  -> do
              -- print prog
              interpret prog
    [] -> print "Expected filename in args"
