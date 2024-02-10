module Specialize where

import Control.Applicative ((<|>))

import AST
import AST2
import Values
import Utils
import Operators
import PrettyPrint
import Division

type Point a = (a, Store)
type Pending a = [(Point a, Point a)]
type Seen a = Pending a

-- How to handle getting the new store for each thing, probably best to shield at end
-- Each block should probably keep its division, otherwise wouldnt be able to label/transfer correctly
-- Block specialization to give the many blocks? 
-- Always make the extra but leave with empty body if not needed
-- Would require the smart path compression (safe with branching still)

-- Result decl 
-- IN {all input initially dynamic} 
-- OUT {all output finally dynamic} 
-- TEMP { all variables that are dynamic at some point but do not occur in in or out}
-- TODO: Above transformation of variable decl
specialize :: (Eq a, Show a) => VariableDecl -> Program' a -> Store -> a -> LEM (Program a (Maybe Store))
specialize decl prog s entry = 
  do b <- raise $ getEntry' prog
     let pending = [((b,s), (entry, emptyStore))]
     res <- specProg entry decl prog pending [] [] 
     decl' <- raise $ specDecl decl prog -- specDecl decl
     return (decl', reverse res) -- Reverse for nicer ordering of blocks

-- specDecl :: VariableDecl' -> VariableDecl
-- specDecl decl = VariableDecl { input = inp, output = out, temp = tmp }
--   where 
--     validate = map fst . filter (\(_,t) -> t == BTDynamic)
--     inp = validate $ input' decl
--     out = validate $ output' decl
--     tmp = validate $ temp' decl
specDecl :: VariableDecl -> Program' a -> EM VariableDecl
specDecl decl p = 
  do inBlock <- getEntryBlock' p
     outBlock <- getExitBlock' p
     let inDiv = initDiv inBlock
     let outDiv = endDiv outBlock
     let inp = filter (\n -> isType n BTDynamic inDiv) $ input decl
     let out = filter (\n -> isType n BTDynamic outDiv) $ output decl
     let potentialTemp = filter (\n -> n `notElem` (inp ++ out)) $ getVarsDecl decl
     let tmp = filter (\n -> any (isDynIn n) p) potentialTemp
     return VariableDecl { input = inp, output = out, temp = tmp }
  where 
    isDynIn n b = isType n BTDynamic (initDiv b) || isType n BTDynamic (endDiv b)


specProg :: (Eq a, Show a) => a -> VariableDecl -> Program' a -> Pending a -> Seen a -> [Block a (Maybe Store)] 
                            -> LEM [Block a (Maybe Store)]
specProg _ _ _ [] _ res = 
  do logM "Specialization done."; return res
specProg entry decl prog (p:ps) seen res 
  | p `elem` seen = 
    do logM $ "REPEAT POINT: " ++ prettyAnn show (fst p)
       specProg entry decl prog ps seen res
  | otherwise = 
    do logM $ prettyAnn show (fst p)
       let (current@(l, s), origin) = p
       b <- raise $ getBlockErr' prog l
       case specBlock entry decl s b origin of
        Left e -> logM ("ERROR: " ++ e ) >> 
                  logM ("IN POINT: " ++ prettyAnn show (fst p)) >>
                    specProg entry decl prog ps seen res
        Right (b', p') -> do
          let pnew = map (\x -> (x, current)) p'
          let seen' = p : seen
          res' <- merge b' res
          specProg entry decl prog (pnew ++ ps) seen' res'

merge :: (Eq a, Show a) => Block a (Maybe Store) -> [Block a (Maybe Store)]
                            -> LEM [Block a (Maybe Store)]
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

specBlock :: Eq a => a -> VariableDecl -> Store -> Block' a -> (a, Store) 
                  -> EM (Block a (Maybe Store), [Point a])
specBlock entry decl s b origin = 
  do let l = (name' b, Just s)
     f <- specFrom entry s (from' b) origin
     (s', as) <- specSteps s $ body' b
     (j, pending) <- specJump s' decl (jump' b) (endDiv b)
     return (Block { name = l, from = f, body = as, jump = j}, pending)

-- TODO: Handle invalid jumps at Spec time or run time, use the annotation?
specFrom :: Eq a => a -> Store -> ComeFrom' a -> (a, Store) 
                 -> EM (ComeFrom a (Maybe Store))
specFrom _ _ (From' l) origin 
  | l `isFrom` origin = return (From (l, Just . snd $ origin))
  | otherwise         = Left "Invalid jump to an unconditional from."
specFrom x s Entry' (l, _) 
  | l == x    = return $ Entry $ Just s
  | otherwise = Left "Invalid jump to entry block."
specFrom _ s (Fi' BTStatic e l1 l2) origin = 
  do v <- getValue e s
     let l = if truthy v then l1 else l2
     if l `isFrom` origin 
      then return . From $ annotate l origin
      else Left "Jump not from expected block."
specFrom _ s (Fi' BTDynamic e l1 l2) origin  
  | l1 `isFrom` origin || l2 `isFrom` origin = 
    do e' <- getExpr e s
       return $ Fi e' (annotate l1 origin) (annotate l2 origin)
  | otherwise = Left "Jump not from either of the expected blocks."

isFrom :: Eq a => a -> (a, Store) -> Bool
isFrom l (l', _) = l == l' -- "l = label(l')" in judgements

annotate :: Eq a => a -> (a, Store) -> (a, Maybe Store)
annotate l (l', s) | l == l'   = (l, Just s)
                   | otherwise = (l, Nothing)

specJump :: Store -> VariableDecl -> Jump' a -> Division
            -> EM (Jump a (Maybe Store), [Point a])
specJump s decl Exit' d = 
  let s' = updateStore s d
      checkable = staticNonOutput decl s'
      vals = map (`find` s') checkable
  in 
  do vs <- sequence vals
     let pairs = zip checkable vs
     let offendingVars = filter (\(_,v) -> v /= Nil) pairs
     case offendingVars of
      [] -> return (Exit (Just s'), [])
      ((n,_):_) -> Left $ "Non-nil non-output variable \"" ++ n ++ "\" at exit"
specJump s _ (Goto' l) d = 
  let s' = updateStore s d
  in return (Goto (l, Just s'), [(l, s')])
specJump s _ (If' BTDynamic e l1 l2) d = 
  let s' = updateStore s d
  in 
  do e' <- getExpr e s'; 
     return (If e' (l1, Just s') (l2, Just s'), [(l1, s'), (l2, s')]) 
specJump s _ (If' BTStatic e l1 l2) d = 
  let s' = updateStore s d
  in 
  do v <- getValue e s
     return $ 
      if truthy v 
        then (Goto (l1, Just s'), [(l1, s')]) 
        else (Goto (l2, Just s'), [(l2, s')])

updateStore :: Store -> Division -> Store
updateStore s d = mapStore updateVar s
  where
    updateVar n v = case getType n d of
      BTStatic -> v
      BTDynamic -> Dynamic  

specSteps :: Store -> [Step'] -> EM (Store, [Step])
specSteps s [] = return (s, [])
specSteps s (a:as) = 
  do (s'', a') <- specStep s a
     (s', as') <- specSteps s'' as
     return (s', a' ++ as')
    
specStep :: Store -> Step' -> EM (Store, [Step])
specStep s (Skip' BTDynamic) = return (s, [Skip])
specStep s (Skip' BTStatic) = return (s, [])
specStep s (Assert' BTDynamic e) = 
  do e' <- getExpr e s
     return (s, [Assert e'])
specStep s (Assert' BTStatic e) = 
  do v <- getValue e s
     if truthy v
      then return (s, [])
      else Left "Assert failed."
specStep s (Update' BTDynamic n op e) = 
  do e' <- getExpr e s
     return (s, [Update n op e'])
specStep s (Update' BTStatic n op e) = 
  do rhs <- getValue e $ s `without` n
     lhs <- find n s
     res <- calcR op lhs rhs
     let s' = update n (Static res) s
     return (s', [])
specStep s (Replacement' BTDynamic q1 q2) = 
  do (p, s'', mq2) <- specPatConstruct s q2
     (s', mq1) <- specPatDeconstruct s'' q1 p
     case (mq1, mq2) of
      (Just q1', Just q2') -> return (s', [Replacement q1' q2'])
      _ -> Left "Dynamic replacement did not produce pattern"
specStep s (Replacement' BTStatic q1 q2) = 
  do (p, s'', q2') <- specPatConstruct s q2
     (s', q1') <- specPatDeconstruct s'' q1 p
     case (q1', q2') of
      (Nothing, Nothing) -> return (s', [])
      q -> Left $ "Static replacement produced residual patterns: " ++ show q

data PEPattern = PEStatic Value | PEDynamic | PECons PEPattern PEPattern

specPatConstruct :: Store -> Pattern' -> EM (PEPattern, Store, Maybe Pattern)
specPatConstruct s (QConst' BTDynamic c) = return (PEDynamic, s, return $ QConst c)
specPatConstruct s (QConst' BTStatic c) = return (PEStatic c, s, Nothing)
specPatConstruct s (QVar' BTDynamic n) = return (PEDynamic, s, return $ QVar n)
specPatConstruct s (QVar' BTStatic n) = 
  do v <- find n s
     let s' = update n (Static Nil) s
     return (PEStatic v, s', Nothing) 
specPatConstruct s (QPair' BTDynamic q1 q2) = 
  do (p1, s1, q1') <- specPatConstruct s q1 
     (p2, s2, q2') <- specPatConstruct s1 q2
     return (combinePEPats p1 p2, s2, combinePats q1' q2')
specPatConstruct s (QPair' BTStatic q1 q2) = 
  do (p1, s1, q1') <- specPatConstruct s q1 
     (p2, s2, q2') <- specPatConstruct s1 q2
     case (p1, p2, q1', q2') of
      (PEStatic _, PEStatic _, Nothing, Nothing) -> 
        return (combinePEPats p1 p2, s2, Nothing)
      _ -> Left "Pattern BT-type issue in static pair"


-- static constants can become dynamic from other side of matching
-- we need the pattern from BTA for true annotation
specPatDeconstruct :: Store -> Pattern' -> PEPattern -> EM (Store, Maybe Pattern)
specPatDeconstruct s (QConst' BTDynamic c) _ = return (s, Just $ QConst c)
specPatDeconstruct s (QConst' BTStatic c) p = 
  do v <- valFromPEPat p
     if c == v 
      then return (s, Nothing) 
      else Left "Failed to match static constant"
specPatDeconstruct s (QVar' BTDynamic n) _ = return (s, Just $ QVar n)
specPatDeconstruct s (QVar' BTStatic n) p = 
  do v <- valFromPEPat p
     v' <- find n s
     res <- calcR Xor v v'
     return (update n (Static res) s, Nothing)
specPatDeconstruct s (QPair' _ q1 q2) p = 
  do (p1, p2) <- splitPEPat p
     (s'', q1') <- specPatDeconstruct s q1 p1
     (s', q2') <- specPatDeconstruct s'' q2 p2
     return (s', combinePats q1' q2')

splitPEPat :: PEPattern -> EM (PEPattern, PEPattern)
splitPEPat PEDynamic = return (PEDynamic, PEDynamic)
splitPEPat (PEStatic (Pair v1 v2)) = return (PEStatic v1, PEStatic v2)
splitPEPat (PECons p1 p2) = return (p1, p2)
splitPEPat _ = Left "Failed to deconstruct value further"


valFromPEPat :: PEPattern -> EM Value
valFromPEPat (PEStatic v) = return v
valFromPEPat PEDynamic = Left "Attempt to extract value from dynamic pattern"
valFromPEPat (PECons p1 p2) = 
  do v1 <- valFromPEPat p1
     v2 <- valFromPEPat p2
     return $ Pair v1 v2

combinePEPats :: PEPattern -> PEPattern -> PEPattern
combinePEPats (PEStatic v1) (PEStatic v2) = PEStatic (Pair v1 v2)
combinePEPats PEDynamic PEDynamic = PEDynamic
combinePEPats p1 p2 = PECons p1 p2

combinePats :: Maybe Pattern -> Maybe Pattern -> Maybe Pattern
combinePats Nothing p = p
combinePats p Nothing = p
combinePats (Just q1) (Just q2) = Just $ QPair q1 q2

type PEValue = Either Value Expr
specExpr :: Store -> Expr' -> EM PEValue
specExpr _ (Const' BTDynamic c) = return . Right $ Const c
specExpr _ (Const' BTStatic c) = return $ Left c
specExpr _ (Var' BTDynamic n) = return . Right $ Var n
specExpr s (Var' BTStatic n) = 
  do v <- find n s; return . Left $ v
specExpr s (Op' BTDynamic op e1 e2) = 
  do e1' <- getExpr e1 s; e2' <- getExpr e2 s
     return . Right $ Op op e1' e2'
specExpr s (Op' BTStatic op e1 e2) = 
  do v1 <- getValue e1 s; v2 <- getValue e2 s
     Left <$> calc op v1 v2
specExpr s (UOp' BTDynamic op e) = 
  do e' <- getExpr e s; return . Right $ UOp op e'
specExpr s (UOp' BTStatic op e) = 
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
