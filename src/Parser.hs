module Parser where

import AST
import Values
import Text.ParserCombinators.Parsec

type Label = String

parseStr :: Parser a -> String -> EM a
parseStr p s = case runParser p () "" s of
  Left err -> Left $ show err
  Right res -> Right res

parseProg :: String -> EM (Program Label)
parseProg = parseStr pProg

pProg :: Parser (Program Label)
pProg = whitespace >> many1 pBlock <* eof

pBlock :: Parser (Block Label)
pBlock = 
  do n <- pLabel; f <- pFrom; b <- many pStep; g <- pGoto;
     return Block {name = n, from = f, body = b, jump = g}

pLabel :: Parser String
pLabel = pName <* symbol ":"

pFrom :: Parser (IfFrom Label)
pFrom = 
  do word "entry"; return Entry
  <|> do word "fi"; e <- pExpr; word "from"; l1 <- pName; word "else"
         FromCond e l1 <$> pName
  <|> do word "from"; From <$> pName

pGoto :: Parser (Jump Label)
pGoto = 
  do word "exit"; return Exit
  <|> do word "if"; e <- pExpr; word "goto"; l1 <- pName; word "else"
         If e l1 <$> pName
  <|> do word "goto"; Goto <$> pName

pStep :: Parser Step
pStep = 
  do word "skip"; return Skip
  <|> do word "assert"; symbol "("; e <- pExpr; symbol ")"; return (Assert e)
  <|> try pUpdate
  <|> do p1 <- pPattern; symbol "<-"; Replacement p1 <$> pPattern

pUpdate :: Parser Step
pUpdate = 
  do n <- pName; op <- pOp; Update n op <$> pExpr
  where 
    pOp = do symbol "+="; return Add
        <|> do symbol "-="; return Sub
        <|> do symbol "^="; return Xor

pPattern :: Parser Pattern
pPattern = 
  do QVar <$> pName
  <|> QConst <$> pConstant
  <|> do symbol "("; q1 <- pPattern; 
         symbol "."; q2 <- pPattern; 
         symbol ")"; return $ QPair q1 q2

pExpr :: Parser Expr
pExpr = chainl1 pComparison pRelOp
  where 
    pRelOp = do symbol "&&"; return . Op $ And
          <|> do symbol "||"; return . Op $ Or  

pComparison :: Parser Expr
pComparison = chainl1 pEquation pComp
  where 
    pComp = do symbol "<"; return . Op $ Less
            <|> do symbol ">"; return . Op $ Greater
            <|> do symbol "="; return . Op $ Equal  

pEquation :: Parser Expr
pEquation = chainl1 pTerm pAddOp
  where
    pAddOp = do symbol "+"; return . Op $ ROp Add
            <|> do symbol "-"; return . Op $ ROp Sub  
            <|> do symbol "^"; return . Op $ ROp Xor  

pTerm :: Parser Expr
pTerm = chainl1 pFactor pMulOp
  where
    pMulOp = do symbol "*"; return . Op $ Mul
            <|> do symbol "/"; return . Op $ Div
            <|> do symbol "#"; return . Op $ Index

pFactor :: Parser Expr
pFactor = 
  Const <$> pConstant
  <|> do word "hd"; UOp Hd <$> pExpr
  <|> do word "tl"; UOp Tl <$> pExpr
  <|> Var <$> pName
  <|> do symbol "!"; UOp Not <$> pExpr
  <|> do symbol "("; e <- pExpr; res <- maybeCons e; symbol ")"; return res
  where
    maybeCons e = 
      do symbol "."; Op Cons e <$> pExpr
      <|> return e

pConstant :: Parser Value
pConstant = symbol "'" >> pValue

pValue :: Parser Value
pValue =
  do symbol "("; c1 <- pValue; 
     symbol "."; c2 <- pValue; 
     symbol ")"; return $ Pair c1 c2
  <|> do word "nil"; return Nil
  <|> Atom <$> pName
  <|> Num <$> pNum

parseSpec :: String -> EM Store
parseSpec = parseStr pFile
  where pFile = do whitespace; res <- many pDeclaration; eof; return $ makeStore res

pDeclaration :: Parser (Name, Value)
pDeclaration = do n <- pName; symbol "="; v <- pConstant; return (n,v)

pName :: Parser String
pName = 
  lexeme . try $ 
    do c <- letter; cs <- many pChar; 
       if c:cs `elem` restricted then fail "Restricted Word" else return $ c:cs
  where 
    pChar = alphaNum <|> char '_' <|> char '\''
    restricted = ["from", "fi", "else", "goto", "if", "entry", "exit", 
                  "skip", "hd", "tl", "assert", "nil"]

pNum :: Parser Word
pNum = lexeme . try $ read <$> many1 digit

word :: String -> Parser ()
word s = lexeme . try $ string s >> notFollowedBy alphaNum

symbol :: String -> Parser ()
symbol s = lexeme . void $ string s

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

whitespace :: Parser ()
whitespace = do 
  _ <- many space 
  optional (do comment; whitespace)

comment :: Parser ()
comment = do 
  _ <- try (string "//") 
  _ <- manyTill anyChar eol
  return ()
      where eol = void newline <|> eof

void :: Parser a -> Parser ()
void p = do _ <- p; return () 

