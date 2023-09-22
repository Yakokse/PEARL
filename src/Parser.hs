module Parser (parseProg, parseSpec) where

import AST
import Values
import Text.ParserCombinators.Parsec

type Label = String

-- NOTE: Added Paren to expr, assoc, precedence
-- Consider white space handling, currently lenient

parseStr :: Parser a -> String -> EM a
parseStr p s = case runParser p () "" s of
  Left err -> Left $ show err
  Right res -> Right res

parseProg :: String -> EM (Program Label)
parseProg = parseStr pProg

pProg :: Parser (Program Label)
pProg = do whitespace; res <- many1 pBlock; eof; return res

pBlock :: Parser (Block Label)
pBlock = 
  do n <- pLabel; f <- pFrom; b <- many pStep; g <- pGoto;
     return Block {name = n, from = f, body = b, jump = g}

pLabel :: Parser String
pLabel = do s <- pName; symbol ":"; return s

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
  <|> do p1 <- pPattern; symbol "<-"; Match p1 <$> pPattern

pPattern :: Parser Pattern
pPattern = 
  do PVar <$> pName
  <|> try(PConst <$> pConst)
  <|> do symbol "("; q1 <- pPattern; 
         symbol "."; q2 <- pPattern; 
         symbol ")"; return $ PPair q1 q2

pUpdate :: Parser Step
pUpdate = 
  do lhs <- pLHS; op <- pOp; lhs op <$> pExpr
  where 
    pLHS = do x <- pName; option (UpdateV x) (pIndex x)
    pIndex n = do symbol "["; e <- pExpr; symbol "]"; return $ UpdateA n e
    pOp = do symbol "+="; return Add
        <|> do symbol "-="; return Sub
        <|> do symbol "^="; return Xor

pExpr :: Parser Expr
pExpr = chainl1 pComparison pRelOp
  where 
    pRelOp = do symbol "&&"; return . EOp $ And
          <|> do symbol "||"; return . EOp $ Or  

pComparison :: Parser Expr
pComparison = chainl1 pEquation pComp
  where 
    pComp = do symbol "<"; return . EOp $ Less
            <|> do symbol ">"; return . EOp $ Greater
            <|> do symbol "="; return . EOp $ Equal  

pEquation :: Parser Expr
pEquation = chainl1 pTerm pAddOp
  where
    pAddOp = do symbol "+"; return . EOp $ ROp Add
            <|> do symbol "-"; return . EOp $ ROp Sub  
            <|> do symbol "^"; return . EOp $ ROp Xor  

pTerm :: Parser Expr
pTerm = chainl1 pFactor pMulOp
  where
    pMulOp = do symbol "*"; return . EOp $ Mul
            <|> do symbol "/"; return . EOp $ Div

pFactor :: Parser Expr
pFactor = 
  try $ EConst <$> pConst
  <|> do word "hd"; EHd <$> pExpr
  <|> do word "tl"; ETl <$> pExpr
  <|> do symbol "("; e <- pExpr; symbol ")"; return e
--  <|> do x <- pName; option (Var x) (pIndex x)
--  where
--    pIndex x = do symbol "["; e <- pExpr; symbol "]"; return $ Arr x e

pConst :: Parser Constant
pConst =
  do symbol "'"; Atom <$> pName
  <|> do symbol "("; c1 <- pConst; 
         symbol "."; c2 <- pConst; 
         symbol ")"; return $ Pair c1 c2
  <|> Num <$> pNum

parseSpec :: String -> EM Store
parseSpec = parseStr pFile
  where pFile = do whitespace; res <- many pDeclaration; eof; return $ makeStore res

pDeclaration :: Parser (Name, Value)
pDeclaration = do n <- pName; symbol "="; v <- pValue; return (n,v)

pValue :: Parser Value
pValue = 
  ScalarVal <$> pNum
  <|> do symbol "["; l <- commaSep pNum; symbol "]"; return $ listToArr l 
  <|> do symbol "("; l <- commaSep pNum; symbol ")"; return $ listToStack l 
  where commaSep p  = p `sepBy` symbol ","

pName :: Parser String
pName = 
  lexeme . try $ 
    do c <- letter; cs <- many pChar; 
       if c:cs `elem` restricted then fail "Restricted Word" else return $ c:cs
  where 
    pChar = alphaNum <|> char '_' <|> char '\''
    restricted = ["from", "fi", "else", "goto", "if", "entry", "exit", 
                  "skip", "hd", "tl", "assert"]

pNum :: Parser Word
pNum = lexeme . try $ read <$> many1 digit

word :: String -> Parser ()
word s = lexeme . try $ do _ <- string s; notFollowedBy alphaNum; return ()

symbol :: String -> Parser ()
symbol s = lexeme $ do _ <- string s; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

whitespace :: Parser ()
whitespace = do 
  _ <- many space 
  optional (do comment; whitespace)

comment :: Parser ()
comment = do 
  _ <- try (string "//") 
  _ <- manyTill anyChar eol
  return ()
      where eol = (do _ <- newline; return ()) <|> eof

