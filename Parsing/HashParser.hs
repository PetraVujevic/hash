module Parsing.HashParser (
    parseInput
  , parseCmd
  , parseAssign
  , parseComm
  , parseVar
) where

import Language.Expressions
import Data.Char
import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative hiding (many)

parseInput :: String -> Cmd
parseInput input = case parse parseCmd "" input of
    Left err ->  error "No match."
    Right c -> c
	
parseCmd :: Parser Cmd
parseCmd = try parseAssign <|> parseComm

parseAssign :: Parser Cmd
parseAssign = do
   var <- parseVar
   spaces
   char '='
   spaces
   val <- parseStr
   return Assign {var = var, val = val}
   
parseComm :: Parser Cmd
parseComm = do
   name <- parseVar
   spaces
   args <- sepBy parseStr (char ' ')
   return Cmd {name = name, args = args}

parseVar :: Parser Expr
parseVar = do
  fc <- firstChar
  rest <- many nonFirstChar
  return $ Var (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || elem a "_#")
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a ||  a == '_')
	
parseStr :: Parser Expr
parseStr = do
  xs <- many $ noneOf [' '] 
  return $ Str xs
