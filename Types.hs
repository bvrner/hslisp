module Types where

import Data.Text (Text)

type AST = [LispVal]
type Name = Text
newtype LispFunc = LispFunc { fn :: [LispVal] -> LispVal }

data LispVal
  = Symbol Text
  | List [LispVal]
  | Number Integer
  | String Text
  | Lambda Text LispFunc
  | Function Name LispFunc
  | LispTrue
  | LispFalse
  | Nil

instance Show LispVal where
  show (Symbol s) = show s
  show (List l) = "List: " ++ concatMap show l
  show (Number n) = show n
  show (String s) = show s
  show (Lambda _ _) = "Lambda"
  show (Function n _) = "Function: " ++ show n
  show LispTrue = "true"
  show Nil = "nil"
  show LispFalse = "false"
