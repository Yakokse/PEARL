module Specialize where

import Control.Applicative ((<|>))

import AST
import Values
import Utils
import Operators
import PrettyPrint
import Execute

type Point a = (a, Store)
type Pending a = [(Point a, Point a)]
type Seen a = Pending a

specialize :: (Eq a, Show a) => Program' a -> Store -> a -> LEM (Program (Annotated a))
specialize (decl, prog) s entry = 
  do b <- raise $ getEntry' prog
     let pending = [((b,s), (entry, emptyStore))]
     res <- specProg entry (decl, prog) pending [] [] 
     let decl' = specDecl decl
     return (decl', reverse res) -- Reverse for nicer ordering of blocks

specDecl :: VariableDecl' -> VariableDecl
specDecl decl = VariableDecl { input = inp, output = out, temp = tmp }
  where 
    validate = map fst . filter (\(_,t) -> t == Dynamic)
    inp = validate $ input' decl
    out = validate $ output' decl
    tmp = validate $ temp' decl

specProg :: (Eq a, Show a) => a -> Program' a -> Pending a -> Seen a -> [Block (Annotated a)] 
                            -> LEM [Block (Annotated a)]
specProg _ _ [] _ res = 
  do logM "Specialization done."; return res
specProg entry (decl, prog) (p:ps) seen res 
  | p `elem` seen = 
    do logM $ "REPEAT POINT: " ++ prettyAnn show (fst p)
       specProg entry (decl, prog) ps seen res
  | otherwise = 
    do logM $ prettyAnn show (fst p)
       let (current@(l, s), origin) = p
       b <- raise $ getBlockErr' prog l
       case specBlock entry decl s b origin of
        Left e -> logM ("ERROR: " ++ e ) >> 
                  logM ("IN POINT: " ++ prettyAnn show (fst p)) >>
                    specProg entry (decl, prog) ps seen res
        Right (b', p') -> do
          let pnew = map (\x -> (x, current)) p'
          let seen' = p : seen
          res' <- merge b' res
          specProg entry (decl, prog) (pnew ++ ps) seen' res'

merge :: (Eq a, Show a) => Block (Annotated a) -> [Block (Annotated a)]
                            -> LEM [Block (Annotated a)]
merge block prog 
  | name block `notElem` map name prog = return $ block : prog
  | otherwise = 
    do logM "MERGE"
       b <- raise $ getBlockErr prog $ name block
       b' <- raise $ mergeBlocks b block
       let rest = filter (\x -> name block /= name x) prog
       return $ b' : rest
  where 
    mergeBlocks x y = do j <- mergeFi (from x) (from y); return $ x { from = j }
    mergeFi (Fi e (l1, s1) (l2, s2)) (Fi e' (l1', s1') (l2', s2')) 
      | e == e' && l1 == l1' && l2 == l2' = 
          return $ Fi e (l1, s1 <|> s1') (l2, s2 <|> s2') 
      | otherwise = Left "Failed to merge blocks due to the Fi's being different"
    mergeFi _ _ = Left "Failed to merge blocks due to one or more statement not being a Fi"

specBlock :: Eq a => a -> VariableDecl' -> Store -> Block' a -> (a, Store) 
                                 -> EM (Block (Annotated a), [Point a])
specBlock entry decl s b origin = 
  do let l = (name' b, Just s)
     f <- specFrom entry s (from' b) origin
     (s', as) <- specSteps s $ body' b
     (j, pending) <- specJump s' decl $ jump' b
     return (Block { name = l, from = f, body = as, jump = j}, pending)

-- TODO: Handle invalid jumps at Spec time or run time, use the annotation?
specFrom :: Eq a => a -> Store -> ComeFrom' a -> (a, Store) 
                            -> EM (ComeFrom (Annotated a))
specFrom _ _ (From' l) origin 
  | l `isFrom` origin = return . From $ annotate l origin
  | otherwise         = Left "Invalid jump to an unconditional from."
specFrom x _ Entry' (l, _) 
  | l == x    = return Entry 
  | otherwise = Left "Invalid jump to entry block."
specFrom _ s (Fi' Static e l1 l2) origin = 
  do v <- getValue e s
     let l = if truthy v then l1 else l2
     if l `isFrom` origin 
      then return . From $ annotate l origin
      else Left "Jump not from expected block."
specFrom _ s (Fi' Dynamic e l1 l2) origin  
  | l1 `isFrom` origin || l2 `isFrom` origin = 
    do e' <- getExpr e s
       return $ Fi e' (annotate l1 origin) (annotate l2 origin)
  | otherwise = Left "Jump not from either of the expected blocks."

isFrom :: Eq a => a -> (a, Store) -> Bool
isFrom l (l', _) = l == l' -- "l = label(l')" in judgements

annotate :: Eq a => a -> (a, Store) -> Annotated a
annotate l (l', s) | l == l'   = (l, Just s)
                   | otherwise = (l, Nothing)

specJump :: Store -> VariableDecl' -> Jump' a -> EM (Jump (Annotated a), [Point a])
specJump s decl Exit' = 
  let checkable = staticNonOutput decl
      vals = map (`find` s) checkable
  in 
  do vs <- sequence vals
     let pairs = zip checkable vs
     let offendingVars = filter (\(_,v) -> v /= Nil) pairs
     case offendingVars of
      [] -> return (Exit, [])
      ((n,_):_) -> Left $ "Non-nil non-output variable \"" ++ n ++ "\" at exit"
specJump s _ (Goto' l) = return (Goto (l, Just s), [(l, s)])
specJump s _ (If' Dynamic e l1 l2) = 
  do e' <- getExpr e s; 
     return (If e' (l1, Just s) (l2, Just s), [(l1, s), (l2, s)]) 
specJump s _ (If' Static e l1 l2) = 
  do v <- getValue e s
     return $ 
      if truthy v 
        then (Goto (l1, Just s), [(l1, s)]) 
        else (Goto (l2, Just s), [(l2, s)])

specSteps :: Store -> [Step'] -> EM (Store, [Step])
specSteps s [] = return (s, [])
specSteps s (a:as) = 
  do (s'', a') <- specStep s a
     (s', as') <- specSteps s'' as
     return (s', a' ++ as')
    
specStep :: Store -> Step' -> EM (Store, [Step])
specStep s (Skip' Dynamic) = return (s, [Skip])
specStep s (Skip' Static) = return (s, [])
specStep s (Assert' Dynamic e) = 
  do e' <- getExpr e s
     return (s, [Assert e'])
specStep s (Assert' Static e) = 
  do v <- getValue e s
     if truthy v
      then return (s, [])
      else Left "Assert failed."
specStep s (Update' Dynamic n op e) = 
  do e' <- getExpr e s
     return (s, [Update n op e'])
specStep s (Update' Static n op e) = 
  do rhs <- getValue e $ s `without` n
     lhs <- find n s
     res <- calcR op lhs rhs
     let s' = update n res s
     return (s', [])
specStep s (Replacement' Dynamic q1 q2) = return (s, [Replacement q1 q2])
specStep s (Replacement' Static q1 q2) = 
  do (s', v) <- deconstruct s q2
     s'' <- construct s' v q1
     return (s'', [])

type PEValue = Either Value Expr
specExpr :: Store -> Expr' -> EM PEValue
specExpr _ (Const' Dynamic c) = return . Right $ Const c
specExpr _ (Const' Static c) = return $ Left c
specExpr _ (Var' Dynamic n) = return . Right $ Var n
specExpr s (Var' Static n) = 
  do v <- find n s; return . Left $ v
specExpr s (Op' Dynamic op e1 e2) = 
  do e1' <- getExpr e1 s; e2' <- getExpr e2 s
     return . Right $ Op op e1' e2'
specExpr s (Op' Static op e1 e2) = 
  do v1 <- getValue e1 s; v2 <- getValue e2 s
     Left <$> calc op v1 v2
specExpr s (UOp' Dynamic op e) = 
  do e' <- getExpr e s; return . Right $ UOp op e'
specExpr s (UOp' Static op e) = 
  do v <- getValue e s;
     Left <$> calcU op v
specExpr s (Lift e) = Right . Const <$> getValue e s

getExpr ::  Expr' -> Store -> EM Expr
getExpr e s = 
  do res <- specExpr s e
     case res of
      Right e' -> return e'
      _ -> Left "Expression expected"

getValue ::  Expr' -> Store -> EM Value
getValue e s = 
  do res <- specExpr s e
     case res of
      Left v -> return v
      _ -> Left "Value expected"
