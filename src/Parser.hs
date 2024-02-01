module Parser where

import AST
import Values
import Text.ParserCombinators.Parsec
import Data.Functor (($>), void)

parseStr :: Parser a -> String -> EM a
parseStr p s = case runParser p () "" s of
  Left err -> Left $ show err
  Right res -> Right res

parseProg :: String -> EM (Program Label ())
parseProg = parseStr pProg

pProg :: Parser (Program Label ())
pProg = (,) <$> (whitespace *> pDecl) <*> (many1 pBlock <* eof)

pDecl :: Parser VariableDecl
pDecl = 
  VariableDecl <$> pNames <*> (symbol "->" *> pNames)
               <*> option [] (word "with" *> pNames)
  where pNames = symbol "(" *> many pName <* symbol ")"

pBlock :: Parser (Block Label ())
pBlock = Block <$> pLabel <*> pFrom <*> many pStep <*> pGoto

pLabel :: Parser (Label, ())
pLabel = pLabelName <* symbol ":"

pFrom :: Parser (ComeFrom Label ())
pFrom = choice [
    Entry () <$ word "entry" 
  , Fi <$> (word "fi" *> pExpr) <*> (word "from" *> pLabelName) <*> (word "else" *> pLabelName) 
  , From <$> (word "from" *> pLabelName) 
  ]

pGoto :: Parser (Jump Label ())
pGoto = choice [
    Exit () <$ word "exit"
  , If <$> (word "if" *> pExpr)  <*> (word "goto" *> pLabelName) <*> (word "else" *> pLabelName)
  , Goto <$> (word "goto" *> pLabelName)
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
  , (symbol "(" *> pExpr >>= maybeCons) <* symbol ")"
  ]
  where
    maybeCons e = option e $ Op Cons e <$> (symbol "." *> pExpr)

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

pDeclaration :: Parser (Name, BTValue)
pDeclaration = (,) <$> (pName <* symbol "=") <*> (Static <$> pConstant)

pLabelName :: Parser (Label, ())
pLabelName = (,) <$> pName <*> return ()

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
whitespace = many space *> optional (comment >> whitespace)

comment :: Parser ()
comment = void $ try (string "//") *> manyTill anyChar eol
    where eol = void newline <|> eof

