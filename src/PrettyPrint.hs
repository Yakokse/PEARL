module PrettyPrint where

import AST
import Data.List (intercalate)
import Values
import Division

prettyDiv :: Division -> String
prettyDiv = concatMap (\(n,t) -> n ++ ": " ++ show t ++ "\n") . divisionToList

prettyAnn :: (a -> String) -> Annotated a -> String
prettyAnn f (l, Nothing) = f l ++ "_NULL"
prettyAnn f (l, Just s) = f l ++ serializeStore s 
  where 
    serializeStore = concatMap (\(n, i) -> "_" ++ n ++ "_" ++ serialize i) . storeToList
    serialize = intercalate "_" . map show . valueToList 


prettyStore :: Store -> String       
prettyStore = concatMap (\(n, i) -> n ++ ":" ++ serialize i) . storeToList
  where  
    serialize v = "(" ++ (intercalate "," . map show $ valueToList v) ++ ")"

prettyProg :: (a -> String) -> Program a -> String
prettyProg f = intercalate "\n" . intercalate ["\n"] . map (prettyBlock f)

prettyBlock :: (a -> String) -> Block a -> [String]
prettyBlock f b = 
  (f (name b) ++ ":") : 
  map ('\t' :) 
    (prettyFrom f (from b) 
    ++ map prettyStep (body b) 
    ++ prettyJump f (jump b))

prettyFrom :: (a -> String) -> IfFrom a -> [String]
prettyFrom f (From l) = ["from " ++ f l]
prettyFrom f (FromCond e l1 l2) = 
  [ "fi " ++ prettyExpr e
  , "\tfrom " ++ f l1
  , "\telse " ++ f l2]
prettyFrom _ Entry = ["entry"]

prettyJump :: (a -> String) -> Jump a -> [String]
prettyJump f (Goto l) = ["goto " ++ f l]
prettyJump f (If e l1 l2) = 
  [ "if " ++ prettyExpr e
  , "\tgoto " ++ f l1
  , "\telse " ++ f l2]
prettyJump _ Exit = ["exit"]

prettyStep :: Step -> String
prettyStep (UpdateV n rop e) = n ++ " " ++ prettyROp rop ++ "= " ++ prettyExpr e
prettyStep (UpdateA n e1 rop e2) = 
    n ++ "[" ++ prettyExpr e1 ++ "] " ++ prettyROp rop ++ "= " ++ prettyExpr e2
prettyStep (Push n1 n2) = "push " ++ n1 ++ " " ++ n2
prettyStep (Pop n1 n2) = "pop " ++ n1 ++ " " ++ n2
prettyStep Skip = "skip"

prettyExpr :: Expr -> String
prettyExpr (Const i)     = show i
prettyExpr (Var n)       = n
prettyExpr (Arr n e)     = n ++ "[" ++ prettyExpr e ++ "]"
prettyExpr (Op op e1 e2) = 
  "(" ++ prettyExpr e1 
    ++ " " ++ prettyOp op 
    ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (Top n)       = "top " ++ n
prettyExpr (Empty n)     = "empty " ++ n
    
prettyOp :: BinOp -> String
prettyOp (ROp op) = prettyROp op
prettyOp Mul      = "*"
prettyOp Div      = "/"
prettyOp And      = "&&"
prettyOp Or       = "||"
prettyOp Less     = "<"
prettyOp Greater  = ">"
prettyOp Equal    = "="

prettyROp :: RevOp -> String
prettyROp Add = "+"
prettyROp Sub = "-"
prettyROp Xor = "^"

prettyProg' :: (a -> String) -> Program' a -> String
prettyProg' f = intercalate "\n" . intercalate ["\n"] . map (prettyBlock' f)

prettyBlock' :: (a -> String) -> Block' a -> [String]
prettyBlock' f b = 
  (f (name' b) ++ ":") : 
  map ('\t' :) 
    (prettyFrom' f (from' b) 
    ++ map prettyStep' (body' b) 
    ++ prettyJump' f (jump' b))

prettyFrom' :: (a -> String) -> IfFrom' a -> [String]
prettyFrom' f (From' Res l)  = ["%from " ++ f l]
prettyFrom' f (From' Elim l) = ["from " ++ f l]
prettyFrom' f (FromCond' Res e l1 l2) = 
  [ "%fi " ++ prettyExpr' e
  , "\t%from " ++ f l1
  , "\t%else " ++ f l2]
prettyFrom' f (FromCond' Elim e l1 l2) = 
  [ "fi " ++ prettyExpr' e
  , "\tfrom " ++ f l1
  , "\telse " ++ f l2]
prettyFrom' _ (Entry' Res)  = ["%entry"]
prettyFrom' _ (Entry' Elim) = ["entry"]

prettyJump' :: (a -> String) -> Jump' a -> [String]
prettyJump' f (Goto' Res l)  = ["%goto " ++ f l]
prettyJump' f (Goto' Elim l) = ["goto " ++ f l]
prettyJump' f (If' Res e l1 l2) = 
  [ "%if " ++ prettyExpr' e
  , "\t%goto " ++ f l1
  , "\t%else " ++ f l2]
prettyJump' f (If' Elim e l1 l2) = 
  [ "if " ++ prettyExpr' e
  , "\tgoto " ++ f l1
  , "\telse " ++ f l2]
prettyJump' _ (Exit' Res)  = ["%exit"]
prettyJump' _ (Exit' Elim) = ["exit"]

prettyStep' :: Step' -> String
prettyStep' (UpdateV' Res n rop e)  = 
    n ++ " %" ++ prettyROp rop ++ "= " ++ prettyExpr' e
prettyStep' (UpdateV' Elim n rop e) = 
    n ++ " " ++ prettyROp rop ++ "= " ++ prettyExpr' e
prettyStep' (UpdateA' Res n e1 rop e2) = 
    n ++ "[" ++ prettyExpr' e1 ++ "] %" ++ prettyROp rop ++ "= " ++ prettyExpr' e2
prettyStep' (UpdateA' Elim n e1 rop e2) = 
    n ++ "[" ++ prettyExpr' e1 ++ "] " ++ prettyROp rop ++ "= " ++ prettyExpr' e2
prettyStep' (Push' Res n1 n2)  = "%push " ++ n1 ++ " " ++ n2
prettyStep' (Push' Elim n1 n2) = "push "  ++ n1 ++ " " ++ n2
prettyStep' (Pop' Res n1 n2)   = "%pop "  ++ n1 ++ " " ++ n2
prettyStep' (Pop' Elim n1 n2)  = "pop "   ++ n1 ++ " " ++ n2
prettyStep' (Skip' Res) = "%skip"
prettyStep' (Skip' Elim) = "skip"

prettyExpr' :: Expr' -> String
prettyExpr' (Const' Res i)   = "%" ++ show i
prettyExpr' (Const' Elim i)  = show i
prettyExpr' (Var' Res n)     = "%" ++ n
prettyExpr' (Var' Elim n)    = n
prettyExpr' (Arr' Res n e)   = "%" ++ n ++ "[" ++ prettyExpr' e ++ "]"
prettyExpr' (Arr' Elim n e)  = n ++ "[" ++ prettyExpr' e ++ "]"
prettyExpr' (Op' Res op e1 e2) = 
    "%(" ++ prettyExpr' e1 ++ " %" ++ prettyOp op ++ " " ++ prettyExpr' e2 ++ ")"
prettyExpr' (Op' Elim op e1 e2) = 
    "(" ++ prettyExpr' e1 ++ " " ++ prettyOp op ++ " " ++ prettyExpr' e2 ++ ")"
prettyExpr' (Top' Res n)     = "%top " ++ n
prettyExpr' (Top' Elim n)    = "top " ++ n
prettyExpr' (Empty' Res n)   = "%empty " ++ n
prettyExpr' (Empty' Elim n)  = "empty " ++ n
prettyExpr' (Lift e)         = "$(" ++ prettyExpr' e ++ ")"

prepend :: a -> [a] -> [a]
prepend _ [] = []
prepend x (y:ys) = x : y : prepend x ys