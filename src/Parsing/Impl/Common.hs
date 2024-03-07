module Parsing.Impl.Common where

import Text.Parsec
import Data.Functor (void)

import Utils.Error

import RL.Values

type Parser = Parsec String ()

-- apply a parser to a string
parseStr :: Parser a -> String -> EM a
parseStr p s = case parse (p <* eof) "" s of
  Left err -> Left $ show err
  Right res -> Right res

-- parse a constant literal
pConstant :: Parser Value
pConstant = symbol "'" *> pValue
 where
  pValue = choice
    [ Pair <$> (symbol "(" *> pValue) <*> (symbol "." *> pValue <* symbol ")")
    , Nil <$ word "nil"
    , Atom <$> pName
    , Num <$> pNum
    ] <?> "Expecting a value"

-- parse a variable name
pName :: Parser String
pName =
  lexeme . try $
    do c <- letter; cs <- many pChar;
       let n = c : cs
       if n `elem` restricted then fail "Restricted Word" else return n
  where
    pChar = choice [alphaNum, char '_', char '\'']
    restricted = ["from", "fi", "else", "goto", "if", "entry", "exit",
                  "skip", "hd", "tl", "assert", "nil", "with"]

-- parse a number
pNum :: Parser Word
pNum = lexeme . try $ read <$> many1 digit

-- parse a specific word, fails if word is only partial
word :: String -> Parser ()
word s = lexeme . try $ string s *> notFollowedBy alphaNum

-- parse a symbol and ignore output
symbol :: String -> Parser ()
symbol s = lexeme . void $ string s

-- parse something then consume whitespace
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- parse all whitespace, including comment
whitespace :: Parser ()
whitespace = many space *> optional (comment >> whitespace)

-- parse a comment
comment :: Parser ()
comment = void $ try (string "//") *> manyTill anyChar eol
    where eol = void newline <|> eof
