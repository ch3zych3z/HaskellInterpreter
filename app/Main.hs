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
      prelude <- readFile "prelude.hs"
      code    <- readFile fileName
      case parseProgram code of
        Left err   -> printError "Parse error:" $ show err
        Right prog -> do
          let prog' = parsePrelude prelude prog
          case inferType prog' of
            Left err -> printError "Type inference error:" err
            Right _  -> do
              interpret prog'
    [] -> print "Expected filename in args"
