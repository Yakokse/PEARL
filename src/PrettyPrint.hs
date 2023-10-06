module PrettyPrint where

import AST
import Data.List (intercalate)
import Values
import Division

type Print a = a -> String

prettyDiv :: Division -> String
prettyDiv = concatMap (\(n,t) -> n ++ ": " ++ prettyLvl t ++ "\n") . divisionToList
  where prettyLvl Static = "Static"
        prettyLvl Dynamic = "Dynamic"

serializeAnn :: Print a -> Annotated a -> String
serializeAnn f (l, Nothing) = f l ++ "_NULL"
serializeAnn f (l, Just s) = f l ++ serializeStore s 
  where 
    serializeStore = concatMap (\(n, i) -> "_" ++ n ++ "_" ++ serialize i) . storeToList
    serialize (Atom a) = a
    serialize (Num i) = show i
    serialize (Pair v1 v2) = "ZS"++ serialize v1 ++ "_" ++ serialize v2 ++ "ZE"
    serialize Nil = "nil"

prettyAnn :: Print a -> (a, Store) -> String
prettyAnn f (l, s) = f l ++ ": " ++ prettyStore s

prettyStore :: Store -> String       
prettyStore = concatMap (\(n, v) -> n ++ "=" ++ prettyVal v ++ " ") . storeToList

prettyProg :: Print a -> Program a -> String
prettyProg f (decl, p)= prettyDecl decl ++ intercalate "\n" (concatMap (prettyBlock f) p)

prettyDecl :: VariableDecl -> String
prettyDecl d = 
  "(" ++ unwords (input d) ++ ") -> (" ++ unwords (output d) ++ ")" ++ tmp
 where tmp = if null (temp d) then "\n\n" else " with (" ++ unwords (temp d) ++ ")\n\n"

prettyBlock :: Print a -> Block a -> [String]
prettyBlock f b = 
  (f (name b) ++ ":") : 
  map ('\t' :) 
    (prettyFrom f (from b) 
    ++ map prettyStep (body b) 
    ++ prettyJump f (jump b)) ++ [""]

prettyFrom :: Print a -> ComeFrom a -> [String]
prettyFrom f (From l) = ["from " ++ f l]
prettyFrom f (Fi e l1 l2) = 
  [ "fi " ++ prettyExpr e
  , "\tfrom " ++ f l1
  , "\telse " ++ f l2]
prettyFrom _ Entry = ["entry"]

prettyJump :: Print a -> Jump a -> [String]
prettyJump f (Goto l) = ["goto " ++ f l]
prettyJump f (If e l1 l2) = 
  [ "if " ++ prettyExpr e
  , "\tgoto " ++ f l1
  , "\telse " ++ f l2]
prettyJump _ Exit = ["exit"]

prettyStep :: Step -> String
prettyStep (Update n rop e) = n ++ " " ++ prettyROp rop ++ "= " ++ prettyExpr e
prettyStep Skip = "skip"
prettyStep (Assert e) = "assert(" ++ prettyExpr e ++ ")"
prettyStep (Replacement q1 q2) = prettyPat q1 ++ " <- " ++ prettyPat q2

prettyPat :: Pattern -> String
prettyPat (QConst v) = prettyVal v
prettyPat (QVar n) = n
prettyPat (QPair q1 q2) = "(" ++ prettyPat q1 ++ " . " ++ prettyPat q2 ++ ")"

prettyExpr :: Expr -> String
prettyExpr (Const v)     = "'" ++ prettyVal v
prettyExpr (Var n)       = n
prettyExpr (Op op e1 e2) = 
  "(" ++ prettyExpr e1 
    ++ " " ++ prettyOp op 
    ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (UOp op e)    =
  prettyUOp op ++ "(" ++ prettyExpr e ++ ")"
    
prettyOp :: BinOp -> String
prettyOp (ROp op) = prettyROp op
prettyOp Mul      = "*"
prettyOp Div      = "/"
prettyOp And      = "&&"
prettyOp Or       = "||"
prettyOp Less     = "<"
prettyOp Greater  = ">"
prettyOp Equal    = "="
prettyOp Cons     = "."
prettyOp Index    = "#"

prettyROp :: RevOp -> String
prettyROp Add = "+"
prettyROp Sub = "-"
prettyROp Xor = "^"

prettyUOp :: UnOp -> String
prettyUOp Hd = "hd"
prettyUOp Tl = "tl"
prettyUOp Not = "!"

prettyVal :: Value -> String
prettyVal (Atom a) = a
prettyVal (Num i) = show i
prettyVal (Pair v1 v2) = "("++ prettyVal v1 ++ "." ++ prettyVal v2 ++ ")"
prettyVal Nil = "nil"

prettyProg' :: Print a -> Program' a -> String
prettyProg' f (decl, p) = prettyDecl' decl ++ intercalate "\n" (concatMap (prettyBlock' f) p)

prettyDecl' :: VariableDecl' -> String
prettyDecl' d = "(" ++ unwords (annVars $ input' d) ++ ") -> (" ++ unwords (annVars $ input' d) ++ ")" ++ tempVars
 where 
  tempVars = if null (temp' d) then "\n\n" else unwords (annVars $ temp' d) ++ "\n\n"
  annVars = map (\(n,t) -> case t of Dynamic -> '%':n; _ -> n)


prettyBlock' :: Print a -> Block' a -> [String]
prettyBlock' f b = 
  (f (name' b) ++ ":") : 
  map ('\t' :) 
    (prettyFrom' f (from' b) 
    ++ map prettyStep' (body' b) 
    ++ prettyJump' f (jump' b)) ++ [""]

prettyFrom' :: Print a -> ComeFrom' a -> [String]
prettyFrom' f (From' l)  = ["from " ++ f l]
prettyFrom' f (Fi' Dynamic e l1 l2) = 
  [ "%fi " ++ prettyExpr' e
  , "\t%from " ++ f l1
  , "\t%else " ++ f l2]
prettyFrom' f (Fi' Static e l1 l2) = 
  [ "fi " ++ prettyExpr' e
  , "\tfrom " ++ f l1
  , "\telse " ++ f l2]
prettyFrom' _ Entry' = ["entry"]

prettyJump' :: Print a -> Jump' a -> [String]
prettyJump' f (Goto' l) = ["goto " ++ f l]
prettyJump' f (If' Dynamic e l1 l2) = 
  [ "%if " ++ prettyExpr' e
  , "\t%goto " ++ f l1
  , "\t%else " ++ f l2]
prettyJump' f (If' Static e l1 l2) = 
  [ "if " ++ prettyExpr' e
  , "\tgoto " ++ f l1
  , "\telse " ++ f l2]
prettyJump' _ Exit' = ["exit"]

prettyStep' :: Step' -> String
prettyStep' (Update' Dynamic n rop e)  = 
    n ++ " %" ++ prettyROp rop ++ "= " ++ prettyExpr' e
prettyStep' (Update' Static n rop e) = 
    n ++ " " ++ prettyROp rop ++ "= " ++ prettyExpr' e
prettyStep' (Skip' Dynamic) = "%skip"
prettyStep' (Skip' Static) = "skip"
prettyStep' (Assert' Dynamic e) = "%assert(" ++ prettyExpr' e ++ ")"
prettyStep' (Assert' Static e) = "assert(" ++ prettyExpr' e ++ ")"
prettyStep' (Replacement' Dynamic q1 q2) = prettyPat q1 ++ " %<- " ++ prettyPat q2
prettyStep' (Replacement' Static q1 q2) = prettyPat q1 ++ " <- " ++ prettyPat q2


prettyExpr' :: Expr' -> String
prettyExpr' (Const' Dynamic i)   = "%'" ++ prettyVal i
prettyExpr' (Const' Static i)  = "'" ++ prettyVal i
prettyExpr' (Var' Dynamic n)     = "%" ++ n
prettyExpr' (Var' Static n)    = n
prettyExpr' (Op' Dynamic op e1 e2) = 
    "%(" ++ prettyExpr' e1 ++ " %" ++ prettyOp op ++ " " ++ prettyExpr' e2 ++ ")"
prettyExpr' (Op' Static op e1 e2) = 
    "(" ++ prettyExpr' e1 ++ " " ++ prettyOp op ++ " " ++ prettyExpr' e2 ++ ")"
prettyExpr' (Lift e)         = "$(" ++ prettyExpr' e ++ ")"
prettyExpr' (UOp' Dynamic op e)    =
  "%" ++ prettyUOp op ++ "(" ++ prettyExpr' e ++ ")"
prettyExpr' (UOp' Static op e)    =
  prettyUOp op ++ "(" ++ prettyExpr' e ++ ")"
prepend :: a -> [a] -> [a]
prepend _ [] = []
prepend x (y:ys) = x : y : prepend x ys