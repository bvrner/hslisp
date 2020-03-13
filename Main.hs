{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser

import Control.Monad (unless)
import System.IO (isEOF, hFlush, stdout)

import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  TI.putStr "user> "
  hFlush stdout
  eof <- isEOF
  unless eof $ do
    input <- TI.getLine
    case readStr input of
      Right ast -> TI.putStrLn $ T.append "AST: " (T.pack $ show ast)
      Left err -> TI.putStrLn err
    main
