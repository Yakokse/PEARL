module PrettyPrint where

import AST
import AST2
import Data.List (intercalate)
import Values
import Division

type Print a = a -> String

prettyDiv :: Division -> String
prettyDiv = concatMap (\(n,t) -> n ++ ": " ++ prettyLvl t ++ "\n") . divisionToList
  where prettyLvl BTStatic = "BTStatic"
        prettyLvl BTDynamic = "BTDynamic"

serializeAnn :: Print a -> a -> Maybe Store -> String
serializeAnn f l Nothing = f l
serializeAnn f l (Just s) = f l ++ serializeStore s 
  where 
    serializeStore = concatMap (\(n, i) -> "_" ++ n ++ "_" ++ serializeBT i) . storeToList
    serializeBT (Static v) = "S" ++ serialize v
    serializeBT Dynamic = "Dyn"
    serialize (Atom a) = a
    serialize (Num i) = show i
    serialize (Pair v1 v2) = "ZS"++ serialize v1 ++ "_" ++ serialize v2 ++ "ZE"
    serialize Nil = "nil"

prettyAnn :: Print a -> (a, Store) -> String
prettyAnn f (l, s) = f l ++ ": " ++ prettyStore s

prettyStore :: Store -> String       
prettyStore = concatMap (\(n, v) -> n ++ "=" ++ prettyBTVal v ++ " ") . storeToList

prettyProg :: Print a -> Program a () -> String
prettyProg f (decl, p)= prettyDecl decl ++ intercalate "\n" (concatMap (prettyBlock f) p)

prettyDecl :: VariableDecl -> String
prettyDecl d = 
  "(" ++ unwords (input d) ++ ") -> (" ++ unwords (output d) ++ ")" ++ tmp
 where tmp = if null (temp d) then "\n\n" else " with (" ++ unwords (temp d) ++ ")\n\n"

prettyBlock :: Print a -> Block a () -> [String]
prettyBlock f b = 
  (f ((fst . name) b) ++ ":") : 
  map ('\t' :) 
    (prettyFrom f (from b) 
    ++ map prettyStep (body b) 
    ++ prettyJump f (jump b)) ++ [""]

prettyFrom :: Print a -> ComeFrom a () -> [String]
prettyFrom f (From (l, ())) = ["from " ++ f l]
prettyFrom f (Fi e (l1, ()) (l2, ())) = 
  [ "fi " ++ prettyExpr e
  , "\tfrom " ++ f l1
  , "\telse " ++ f l2]
prettyFrom _ (Entry ()) = ["entry"]

prettyJump :: Print a -> Jump a () -> [String]
prettyJump f (Goto (l, ()))= ["goto " ++ f l]
prettyJump f (If e (l1, ()) (l2, ())) = 
  [ "if " ++ prettyExpr e
  , "\tgoto " ++ f l1
  , "\telse " ++ f l2]
prettyJump _ (Exit ()) = ["exit"]

prettyStep :: Step -> String
prettyStep (Update n rop e) = n ++ " " ++ prettyROp rop ++ "= " ++ prettyExpr e
prettyStep Skip = "skip"
prettyStep (Assert e) = "assert(" ++ prettyExpr e ++ ")"
prettyStep (Replacement q1 q2) = prettyPat q1 ++ " <- " ++ prettyPat q2

prettyPat :: Pattern -> String
prettyPat (QConst v) = "'" ++ prettyVal v
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

prettyBTVal :: SpecValue -> String
prettyBTVal (Static v) = "S" ++ prettyVal v
prettyBTVal Dynamic    = "Dyn"

prettyVal :: Value -> String
prettyVal (Atom a) = a
prettyVal (Num i) = show i
prettyVal (Pair v1 v2) = "("++ prettyVal v1 ++ "." ++ prettyVal v2 ++ ")"
prettyVal Nil = "nil"

prettyProg' :: Print a -> Program' a -> String
prettyProg' f p = intercalate "\n" (concatMap (prettyBlock' f) p)

prettyBlock' :: Print a -> Block' a -> [String]
prettyBlock' f b = 
  (f (name' b) ++ ":") : 
  map ('\t' :) 
    (prettyFrom' f (from' b) 
    ++ map prettyStep' (body' b) 
    ++ prettyJump' f (jump' b)) ++ [""]

prettyFrom' :: Print a -> ComeFrom' a -> [String]
prettyFrom' f (From' l)  = ["%from " ++ f l]
prettyFrom' f (Fi' BTDynamic e l1 l2) = 
  [ "%fi " ++ prettyExpr' e
  , "\t%from " ++ f l1
  , "\t%else " ++ f l2]
prettyFrom' f (Fi' BTStatic e l1 l2) = 
  [ "fi " ++ prettyExpr' e
  , "\tfrom " ++ f l1
  , "\telse " ++ f l2]
prettyFrom' _ Entry' = ["%entry"]

prettyJump' :: Print a -> Jump' a -> [String]
prettyJump' f (Goto' l) = ["%goto " ++ f l]
prettyJump' f (If' BTDynamic e l1 l2) = 
  [ "%if " ++ prettyExpr' e
  , "\t%goto " ++ f l1
  , "\t%else " ++ f l2]
prettyJump' f (If' BTStatic e l1 l2) = 
  [ "if " ++ prettyExpr' e
  , "\tgoto " ++ f l1
  , "\telse " ++ f l2]
prettyJump' _ Exit' = ["%exit"]

prettyStep' :: Step' -> String
prettyStep' (Update' BTDynamic n rop e)  = 
    n ++ " %" ++ prettyROp rop ++ "= " ++ prettyExpr' e
prettyStep' (Update' BTStatic n rop e) = 
    n ++ " " ++ prettyROp rop ++ "= " ++ prettyExpr' e
prettyStep' (Skip' BTDynamic) = "%skip"
prettyStep' (Skip' BTStatic) = "skip"
prettyStep' (Assert' BTDynamic e) = "%assert(" ++ prettyExpr' e ++ ")"
prettyStep' (Assert' BTStatic e) = "assert(" ++ prettyExpr' e ++ ")"
prettyStep' (Replacement' BTDynamic q1 q2) = prettyPat' q1 ++ " %<- " ++ prettyPat' q2
prettyStep' (Replacement' BTStatic q1 q2) = prettyPat' q1 ++ " <- " ++ prettyPat' q2

prettyPat' :: Pattern' -> String
prettyPat' (QConst' BTDynamic v) = "%'" ++ prettyVal v
prettyPat' (QConst' BTStatic v) = "'" ++ prettyVal v
prettyPat' (QVar' BTDynamic n) = "%" ++ n
prettyPat' (QVar' BTStatic n) =  n
prettyPat' (QPair' BTDynamic q1 q2) = "%(" ++ prettyPat' q1 ++ " %. " ++ prettyPat' q2 ++ ")"
prettyPat' (QPair' BTStatic q1 q2) = "(" ++ prettyPat' q1 ++ " . " ++ prettyPat' q2 ++ ")"
prettyPat' (QLift q) = "lift(" ++ prettyPat' q ++ ")"

prettyExpr' :: Expr' -> String
prettyExpr' (Const' BTDynamic i)   = "%'" ++ prettyVal i
prettyExpr' (Const' BTStatic i)  = "'" ++ prettyVal i
prettyExpr' (Var' BTDynamic n)     = "%" ++ n
prettyExpr' (Var' BTStatic n)    = n
prettyExpr' (Op' BTDynamic op e1 e2) = 
    "%(" ++ prettyExpr' e1 ++ " %" ++ prettyOp op ++ " " ++ prettyExpr' e2 ++ ")"
prettyExpr' (Op' BTStatic op e1 e2) = 
    "(" ++ prettyExpr' e1 ++ " " ++ prettyOp op ++ " " ++ prettyExpr' e2 ++ ")"
prettyExpr' (Lift e)         = "$(" ++ prettyExpr' e ++ ")"
prettyExpr' (UOp' BTDynamic op e)    =
  "%" ++ prettyUOp op ++ "(" ++ prettyExpr' e ++ ")"
prettyExpr' (UOp' BTStatic op e)    =
  prettyUOp op ++ "(" ++ prettyExpr' e ++ ")"

prepend :: a -> [a] -> [a]
prepend _ [] = []
prepend x (y:ys) = x : y : prepend x ys
