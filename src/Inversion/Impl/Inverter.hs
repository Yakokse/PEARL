module Inversion.Impl.Inverter where

import RL.AST

-- invert a program
invertProg :: Program a b -> Program a b
invertProg  = map invertProc


-- invert a proc
invertProc :: Procedure a b -> Procedure a b
invertProc = undefined --TODO: implement


-- invert a block
invertBlock :: Block a b -> Block a b
invertBlock Block {name = l, from = f, body = b, jump = j} = Block
  { name = l,
    from = invertJump j,
    body = reverse $ map invertStep b,
    jump = invertFrom f}

-- invert a come-from
invertFrom :: ComeFrom a b -> Jump a b
invertFrom (Entry p s)          = Exit p s -- TODO: correct?
invertFrom (From l)         = Goto l
invertFrom (Fi e l1 l2) = If e l1 l2

-- invert a jump
invertJump :: Jump a b -> ComeFrom a b
invertJump (Exit p s)           = Entry p s --TODO: correct?
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
