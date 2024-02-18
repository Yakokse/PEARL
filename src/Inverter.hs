module Inverter (invertProg) where
import AST

-- invert a program
invertProg :: Program a b -> Program a b
invertProg (decl, p) = (invDecl, map invertBlock p)
  where 
    invDecl = decl {input = output decl, output = input decl} 

-- invert a block
invertBlock :: Block a b -> Block a b
invertBlock Block {name = l, from = f, body = b, jump = j} = Block 
  { name = l, 
    from = invertJump j, 
    body = reverse $ map invertStep b,
    jump = invertFrom f}

-- invert a come-from
invertFrom :: ComeFrom a b -> Jump a b
invertFrom (Entry s)          = Exit s
invertFrom (From l)         = Goto l
invertFrom (Fi e l1 l2) = If e l1 l2

-- invert a jump
invertJump :: Jump a b -> ComeFrom a b
invertJump (Exit s)           = Entry s
invertJump (Goto l)         = From l
invertJump (If e l1 l2) = Fi e l1 l2

-- invert a step
invertStep :: Step -> Step
invertStep Skip = Skip
invertStep (Assert e) = Assert e
invertStep (Replacement q1 q2) = Replacement q2 q1
invertStep (Update n op e) = Update n (invertOp op) e

-- invert a reversible operation
invertOp :: RevOp -> RevOp
invertOp Add = Sub
invertOp Sub = Add
invertOp Xor = Xor
