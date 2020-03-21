{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Eval

import Data.Text (pack)

import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      input <- getInputLine "user> "
      case input of
        Nothing -> outputStrLn "Goodbye!"
        Just str ->
          case readStr (pack str) of
            Right ast -> repl ast >> loop
            Left err -> outputStr err >> loop

    repl :: [LispVal] -> InputT IO ()
    repl [] = return ()
    repl (x:xs) = 
      case runEval replEnv (eval' x) of
        Right ev -> outputStrLn (show ev) >> repl xs
        Left err -> outputStrLn err >> repl xs
