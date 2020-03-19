{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Eval

import Control.Monad (unless)
import System.IO (isEOF, hFlush, stdout)

import qualified Data.Text.IO as TI

main :: IO ()
main = do
  TI.putStr "user> "
  hFlush stdout
  eof <- isEOF
  unless eof $ do
    input <- TI.getLine
    case readStr input of
      Right ast -> repl ast
      Left err -> TI.putStrLn err
    main
      where
        repl [] = return ()
        repl (x:xs) = 
          case runEval replEnv (eval' x) of
            Right ev -> print ev >> repl xs
            Left err -> print err >> repl xs
