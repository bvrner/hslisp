{-# LANGUAGE OverloadedStrings #-}

module Parser (readStr, module Types) where

import Types

import Data.Void (Void)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec.Char
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

readStr :: Text -> Either Text [LispVal]
readStr t =
  case parse pLisp "f" t of
    Right parsed -> Right parsed
    Left err -> Left $ T.pack $ errorBundlePretty err

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol' :: Text -> Parser Text
symbol' = L.symbol' sc

pNil :: Parser LispVal
pNil = symbol' "nil" >> return Nil

integer :: Parser Integer
integer = lexeme L.decimal

lispSymbols :: Parser Char
lispSymbols = oneOf ("#$%&|*+-/:<=>?@^_~" :: String)

pLispVal :: Parser LispVal
pLispVal = choice [pList, pNumber, pSymbol, pNil, pString]

pSymbol :: Parser LispVal
pSymbol = (Symbol . T.pack <$> lexeme (some (letterChar <|> lispSymbols)))

pList :: Parser LispVal
pList = List <$> between (symbol "(") (symbol ")") (many pLispVal)

pLisp :: Parser [LispVal]
pLisp = some pLispVal

pNumber :: Parser LispVal
pNumber = Number <$> integer

pString :: Parser LispVal
pString = do
  str <- char '\"' *> manyTill L.charLiteral (char '\"')
  return $ String (T.pack str)
