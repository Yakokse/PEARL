module Parser where

import AST
import Values
import Text.ParserCombinators.Parsec
import Data.Functor (($>), void)
type Label = String

parseStr :: Parser a -> String -> EM a
parseStr p s = case runParser p () "" s of
  Left err -> Left $ show err
  Right res -> Right res

parseProg :: String -> EM (Program Label)
parseProg = parseStr pProg

pProg :: Parser (Program Label)
pProg = (,) <$> (whitespace *> pDecl) <*> (many1 pBlock <* eof)

pDecl :: Parser VariableDecl
pDecl = 
  do inp <- pNames; symbol "->"; out <- pNames; tmp <- option [] (word "with" *> pNames)
     return VariableDecl {input=inp, output=out, temp=tmp}
  where pNames = symbol "(" *> manyTill pName (symbol ")")

pBlock :: Parser (Block Label)
pBlock = 
  do n <- pLabel; f <- pFrom; b <- many pStep; g <- pGoto;
     return Block {name = n, from = f, body = b, jump = g}

pLabel :: Parser String
pLabel = pName <* symbol ":"

pFrom :: Parser (ComeFrom Label)
pFrom = choice [
    word "entry" $> Entry
  , Fi <$> (word "fi" *> pExpr) <*> (word "from" *> pName) <*> (word "else" *> pName)
  , From <$> (word "from" *> pName)
  ]

pGoto :: Parser (Jump Label)
pGoto = choice [
    word "exit" $> Exit
  , If <$> (word "if" *> pExpr) <*> (word "goto" *> pName) <*> (word "else" *> pName)
  , Goto <$> (word "goto" *> pName)
  ]

pStep :: Parser Step
pStep = choice [
    word "skip" $> Skip
  , Assert <$> (word "assert" *> symbol "(" *> pExpr <* symbol ")")
  , try pUpdate
  , Replacement <$> pPattern <*> (symbol "<-" *> pPattern)
  ]

pUpdate :: Parser Step
pUpdate = 
  Update <$> pName <*> pOp <*> pExpr
  where 
    pOp = choice [symbol "+=" $> Add, symbol "-=" $> Sub, symbol "^=" $> Xor]

pPattern :: Parser Pattern
pPattern = choice [
    QVar <$> pName
  , QConst <$> pConstant
  , QPair <$> (symbol "(" *> pPattern) <*> (symbol "." *> pPattern <* symbol ")")
  ]

pExpr :: Parser Expr
pExpr = chainl1 pComparison pRelOp
  where 
    pRelOp = choice [ symbol "&&" $> Op And
                    , symbol "||" $> Op Or]

pComparison :: Parser Expr
pComparison = chainl1 pEquation pComp
  where 
    pComp = choice [ symbol "<" $> Op Less
                   , symbol ">" $> Op Greater
                   , symbol "=" $> Op Equal]

pEquation :: Parser Expr
pEquation = chainl1 pTerm pAddOp
  where
    pAddOp = choice [ symbol "+" $> Op (ROp Add)
                    , symbol "-" $> Op (ROp Sub)
                    , symbol "^" $> Op (ROp Xor)]

pTerm :: Parser Expr
pTerm = chainl1 pFactor pMulOp
  where
    pMulOp = choice [ symbol "*" $> Op Mul
                    , symbol "/" $> Op Div
                    , symbol "#" $> Op Index]

pFactor :: Parser Expr
pFactor = choice [
    UOp Hd <$> (word "hd" *> pAtom)
  , UOp Tl <$> (word "tl" *> pAtom)
  , UOp Not <$> (symbol "!" *> pAtom)
  , pAtom
  ]

pAtom :: Parser Expr
pAtom = choice [
    Const <$> pConstant
  , Var <$> pName
  , do symbol "("; e <- pExpr; res <- maybeCons e; symbol ")"; return res
  ]
  where
    maybeCons e = choice [Op Cons e <$> (symbol "." *> pExpr), return e]

pConstant :: Parser Value
pConstant = symbol "'" *> pValue

pValue :: Parser Value
pValue = choice [
    Pair <$> (symbol "(" *> pValue) <*> (symbol "." *> pValue <* symbol ")")
  , word "nil" $> Nil
  , Atom <$> pName
  , Num <$> pNum
  ]

parseSpec :: String -> EM Store
parseSpec = parseStr pFile
  where pFile = makeStore <$> (whitespace *> many pDeclaration <* eof)

pDeclaration :: Parser (Name, Value)
pDeclaration = (,) <$> pName <*> (symbol "=" *> pConstant)

pName :: Parser String
pName = 
  lexeme . try $ 
    do c <- letter; cs <- many pChar; 
       if c:cs `elem` restricted then fail "Restricted Word" else return $ c:cs
  where 
    pChar = choice [alphaNum, char '_', char '\'']
    restricted = ["from", "fi", "else", "goto", "if", "entry", "exit", 
                  "skip", "hd", "tl", "assert", "nil", "with"]

pNum :: Parser Word
pNum = lexeme . try $ read <$> many1 digit

word :: String -> Parser ()
word s = lexeme . try $ string s *> notFollowedBy alphaNum

symbol :: String -> Parser ()
symbol s = lexeme . void $ string s

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

whitespace :: Parser ()
whitespace = do 
  _ <- many space 
  optional (do comment; whitespace)

comment :: Parser ()
comment = void $ try (string "//") *> manyTill anyChar eol
    where eol = void newline <|> eof

