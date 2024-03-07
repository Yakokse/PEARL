module Impl.Parser where

import AST
import Values
import Text.Parsec
import Text.Parsec.Expr
import Data.Functor (($>), void)
import Impl.Maps

type Parser = Parsec String ()

-- apply a parser to a string
parseStr :: Parser a -> String -> EM a
parseStr p s = case parse (p <* eof) "" s of
  Left err -> Left $ show err
  Right res -> Right res

-- parser a string as a program
parseProg :: String -> EM (Program Label ())
parseProg = parseStr pProg

-- parse a program
pProg :: Parser (Program Label ())
pProg = (,) <$> (whitespace *> pDecl) <*> many1 pBlock

-- parse a variable declaration
pDecl :: Parser VariableDecl
pDecl =
  VariableDecl <$> pNames <*> (symbol "->" *> pNames)
               <*> option [] (word "with" *> pNames)
  where pNames = symbol "(" *> many pName <* symbol ")"

-- parse a block
pBlock :: Parser (Block Label ())
pBlock = Block <$> (pLabelName <* symbol ":")
               <*> pFrom
               <*> many pStep
               <*> pJump

-- parse a come-from
pFrom :: Parser (ComeFrom Label ())
pFrom = choice
  [ Entry () <$ word "entry"
  , Fi <$> (word "fi" *> pExpr) <*> (word "from" *> pLabelName) <*> (word "else" *> pLabelName)
  , From <$> (word "from" *> pLabelName)
  ] <?> "Expecting a from"

-- parse a jump
pJump :: Parser (Jump Label ())
pJump = choice
  [ Exit () <$ word "exit"
  , If <$> (word "if" *> pExpr)  <*> (word "goto" *> pLabelName) <*> (word "else" *> pLabelName)
  , Goto <$> (word "goto" *> pLabelName)
  ] <?> "Expecting a jump"

-- parse a step
pStep :: Parser Step
pStep = choice
  [ word "skip" $> Skip
  , Assert <$> (word "assert" *> symbol "(" *> pExpr <* symbol ")")
  , try pUpdate
  , Replacement <$> pPattern <*> (symbol "<-" *> pPattern)
  ] <?> "Expecting a step"

-- parse a reversible update
pUpdate :: Parser Step
pUpdate =
  Update <$> pName <*> pOp <*> pExpr
  where
    pOp = choice [ symbol "+=" $> Add
                 , symbol "-=" $> Sub
                 , symbol "^=" $> Xor] <?> "Expecting reversible update"

-- parse a pattern for a reversible replacement
pPattern :: Parser Pattern
pPattern = choice
  [ QVar <$> pName
  , QConst <$> pConstant
  , QPair <$> (symbol "(" *> pPattern) <*> (symbol "." *> pPattern <* symbol ")")
  ] <?> "Expecting pattern"

pExpr :: Parser Expr
pExpr = buildExpressionParser table term <?> "expression"
 where
  atom = (Const <$> pConstant) <|> (Var <$> pName)
  parens = between (symbol "(") (symbol ")")
  term   = parens pExpr <|> atom
        <?> "simple expression"
  table =
    [ [prefixW "hd" Hd, prefixW "tl" Tl, prefixS "!" Not]
    , [binary "*" Mul, binary "/" Div]
    , [binary "+" (ROp Add), binary "-" (ROp Sub), binary "^" (ROp Xor)]
    , [binary "<" Less, binary ">" Greater, binary "=" Equal]
    , [binary "&&" And, binary "||" Or]
    , [binary "." Cons]
    ]
  binary  op f = Infix (do symbol op; return (Op f)) AssocLeft
  prefixW op f = Prefix $ do word op;   return (UOp f)
  prefixS op f = Prefix $ do symbol op; return (UOp f)

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

-- parse a string as a specialization specification
parseSpec :: String -> EM Store
parseSpec = parseStr pFile
  where pFile = fromList <$> (whitespace *> many pDeclaration)

-- parse a single declaration in a specification
pDeclaration :: Parser (Name, Value)
pDeclaration = (,) <$> (pName <* symbol "=") <*> pConstant

-- parse a label for the abstracted labels in AST
pLabelName :: Parser (Label, ())
pLabelName = (,) <$> pName <*> return ()

-- parse a variable name
pName :: Parser String
pName =
  lexeme . try $
    do c <- letter; cs <- many pChar;
       if c:cs `elem` restricted then fail "Restricted Word" else return $ c:cs
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
