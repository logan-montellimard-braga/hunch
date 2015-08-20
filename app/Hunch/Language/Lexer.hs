module Hunch.Language.Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Control.Applicative ((<*))

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser grammar
  where
    ops = [">", "+", "*"]
    grammar = emptyDef { Token.identStart      = letter
                       , Token.identLetter     = noneOf " )(>+*="
                       , Token.reservedOpNames = ops
                       }

skip :: Parser a -> Parser ()
skip p = return () <* p

betweenDouble :: Parser a -> Parser b -> Parser b
betweenDouble delim = between delim delim

decimal :: Parser Integer
decimal = Token.decimal lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

identifier :: Parser String
identifier = Token.identifier lexer

nonEscape :: Parser String
nonEscape = do
  ch <- noneOf "\\'"
  return [ch]

singleStringEscape :: Parser String
singleStringEscape = do
  skip $ char '\\'
  ch <- char '\''
  return [ch]

character :: Parser String
character = singleStringEscape <|> nonEscape

singleQString :: Parser String
singleQString = do
  strings <- betweenDouble (char '\'') (many1 character <?> "non-empty string")
  spaces
  return $ concat strings

doubleQString :: Parser String
doubleQString = Token.stringLiteral lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
