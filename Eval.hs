{-# LANGUAGE OverloadedStrings  #-}

module Eval where

import Types

import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except

import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

type Eval a = ReaderT Env (ExceptT String Identity) a
type Env = HashMap Text LispVal

replEnv :: Env
replEnv = HM.fromList [("+", Function "+" (LispFunc $ foldr1 (\(Number x) (Number y) -> Number (x + y)))),
                       ("-", Function "-" (LispFunc $ foldl1 (\(Number a) (Number b) -> Number (a - b)))),
                       ("*", Function "*" (LispFunc $ foldr1 (\(Number a) (Number b) -> Number (a * b)))),
                       ("/", Function "/" (LispFunc $ foldl1 (\(Number a) (Number b) -> Number (a `div` b))))]

runEval :: Env -> Eval a -> Either String a
runEval env ev = runIdentity (runExceptT (runReaderT ev env))
{-# INLINABLE runEval #-}

eval' :: LispVal -> Eval LispVal
eval' (Symbol t) = do
  env <- ask
  case HM.lookup t env of
    Just val -> return val
    Nothing -> throwError ("unbound symbol: " ++ show t) 

eval' (List []) = return Nil
eval' (List (x:xs)) = do
  head' <- eval' x
  rest <- mapM eval' xs

  case head' of
    Function _ fun -> return $ fn fun rest
    _ -> return $ List (head' : rest)

eval' v = return v
