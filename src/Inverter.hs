module Inverter where
import AST


invertProg :: Program a -> Program a
invertProg (decl, p) = (invDecl, map invertBlock p)
  where 
    invDecl = decl {input = output decl, output = input decl} 

invertBlock :: Block a -> Block a
invertBlock Block {name = l, from = f, body = b, jump = j} = Block 
  { name = l, 
    from = invertJump j, 
    body = reverse $ map invertStep b,
    jump = invertFrom f}

invertFrom :: ComeFrom a -> Jump a
invertFrom Entry              = Exit
invertFrom (From l)           = Goto l
invertFrom (Fi e l1 l2) = If e l1 l2

invertJump :: Jump a -> ComeFrom a
invertJump Exit         = Entry
invertJump (Goto l)     = From l
invertJump (If e l1 l2) = Fi e l1 l2

invertStep :: Step -> Step
invertStep Skip = Skip
invertStep (Assert e) = Assert e
invertStep (Replacement q1 q2) = Replacement q2 q1
invertStep (Update n op e) = Update n (invertOp op) e

invertOp :: RevOp -> RevOp
invertOp Add = Sub
invertOp Sub = Add
invertOp Xor = Xor