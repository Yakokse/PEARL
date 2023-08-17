module Parser (parseProg) where

import AST
import Text.ParserCombinators.Parsec

type Label = String

-- NOTE: Added Paren to expr, assoc, precedence
-- Consider white space handling, currently lenient

--test p s = 
--    case runParser p () "" s of
--        Left err -> Left $ show err
--        Right res -> Right res

parseProg :: String -> EM (Program Label)
parseProg s = 
    case runParser pProg () "" s of
        Left err -> Left $ show err
        Right res -> Right res

pProg :: Parser (Program Label)
pProg = do res <- many1 pBlock; eof; return res

pBlock :: Parser (Block Label)
pBlock = 
    do n <- pLabel; f <- pFrom; b <- many pStat; g <- pGoto;
       return Block {name = n, from = f, body = b, goto = g}

pLabel :: Parser String
pLabel = do s <- pName; symbol ":"; return s

pFrom :: Parser (IfFrom Label)
pFrom = do word "entry"; return Entry
    <|> do word "fi"; e <- pExpr; word "from"; l1 <- pName; word "else"
           FromCond e l1 <$> pName
    <|> do word "from"; From <$> pName

pGoto :: Parser (IfGoto Label)
pGoto = do word "exit"; return Exit
    <|> do word "if"; e <- pExpr; word "goto"; l1 <- pName; word "else"
           GotoCond e l1 <$> pName
    <|> do word "goto"; Goto <$> pName

pStat :: Parser Statement
pStat = do word "skip"; return Skip
    <|> do word "push"; x <- pName; Push x <$> pName
    <|> do word "pop"; x <- pName; Pop x <$> pName
    <|> do x <- pPlace; op <- pUpdate; Update x op <$> pExpr
    where pUpdate = do symbol "+="; return Add
                <|> do symbol "-="; return Sub
                <|> do symbol "^="; return Xor

pExpr :: Parser Expr
pExpr = chainl1 pEquation pRelOp
    where 
        pRelOp = do symbol "&&"; return . Op $ And
                <|> do symbol "||"; return . Op $ Or  

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

pFactor :: Parser Expr
pFactor = Const <$> pNum
    <|> do word "top"; Top <$> pName
    <|> do word "empty"; Empty <$> pName
    <|> do symbol "("; e <- pExpr; symbol ")"; return e
    <|> Place <$> pPlace

pPlace :: Parser Place
pPlace = do x <- pName; option (Var x) (pIndex x)
    where
        pIndex x = do symbol "["; e <- pExpr; symbol "]"; return $ Arr x e

pName :: Parser String
pName = lexeme . try $ 
    do c <- letter; cs <- many pChar; 
       if c:cs `elem` restricted then fail "Restricted Word" else return $ c:cs
    where 
        pChar = alphaNum <|> char '_'
        restricted = ["from", "fi", "else", "goto", "if", "entry", "exit", 
                      "push", "pop", "skip", "top", "empty"]

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
        where eol = do _ <- newline; return () <|> eof