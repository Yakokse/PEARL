module Parsing.Impl.Parser where

import Utils.Maps
import Utils.Error

import RL.AST
import RL.Values

import Parsing.Impl.Common

import Text.Parsec
import Text.Parsec.Expr

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
  [ Skip <$ word "skip"
  , Assert <$> (word "assert" *> symbol "(" *> pExpr <* symbol ")")
  , try pUpdate
  , Replacement <$> pPattern <*> (symbol "<-" *> pPattern)
  ] <?> "Expecting a step"

-- parse a reversible update
pUpdate :: Parser Step
pUpdate =
  Update <$> pName <*> pOp <*> pExpr
  where
    pOp = choice [ Add <$ symbol "+="
                 , Sub <$ symbol "-="
                 , Xor <$ symbol "^="] <?> "Expecting reversible update"

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
