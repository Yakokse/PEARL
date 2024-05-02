module Assertions.Impl.Analysis where

import Utils.Maps

import RL.AST
import RL.Program
import RL.Values
import RL.Variables

import Assertions.Impl.Abstraction

import qualified Data.List as List
import Control.Monad (foldM)

-- Maybe AStore forms a lattice
-- If any var is bot, whole astore is bot
type AStore = Map Name AValue
type State a b = Map (a,b) (Maybe AStore)

inferProg :: (Ord a, Ord b) => Program a b -> State a b -> State a b
inferProg (decl, prog) preState =
  let postState = fromList $ map (\b -> (name b, Nothing)) prog
      entryLabel = getEntryName prog
      finalState = fixPoint postState [entryLabel]
  in finalState
  where
    fixPoint postState [] = postState
    fixPoint postState (l:ls) =
      let b = getBlockUnsafe prog l
          (newState, pending) =
            inferBlock preState postState (decl, prog) b
          ls' = List.union ls pending
      in fixPoint newState ls'

inferProgWithoutAsserts :: (Ord a, Ord b) => Program a b -> State a b -> State a b
inferProgWithoutAsserts (decl, prog) = inferProg (decl, prog')
  where
    notAssert (Assert _) = False
    notAssert _          = True
    removeAssertion b = b{body = filter notAssert $ body b}
    prog' = map removeAssertion prog

-- Update state and return new pending points
inferBlock :: (Ord a, Ord b) => State a b -> State a b
                             -> Program a b -> Block a b
                             -> (State a b, [(a, b)])
inferBlock preState postState prog
           Block{name = n, from = f, body = b, jump = j} =
    let startStore' = inferFrom postState prog n f
        startStore = get n preState `glbStore` startStore'
        newStore = case startStore of
                        Just s -> foldM inferStep s b
                        Nothing -> Nothing
        pending = if get n postState == newStore
                    then []
                    else jumpLabels j
        newState = set n newStore postState
    in (newState, pending)

inferFrom :: (Ord a, Ord b) => State a b -> Program a b
                            -> (a, b) -> ComeFrom a b
                            -> Maybe AStore
inferFrom _ (decl, _) _ (Entry _) = inferDecl decl

inferFrom state (_, prog) l (From l1) =
  let origS = get l1 state
      j = jump $ getBlockUnsafe prog l1
  in inferJump origS l j
inferFrom state (_, prog) l (Fi e l1 l2) =
  let origS1 = getjump l1
      origS2 = getjump l2
      inferred1 = inferAssertion' origS1 e
      inferred2 = inferAssertion' origS2 (UOp Not e)
  in inferred1 `lubStore` inferred2
  where getjump orig =
          let origS = get orig state
              j = jump $ getBlockUnsafe prog orig
          in inferJump origS l j

inferJump :: (Ord a, Ord b) => Maybe AStore -> (a,b) -> Jump a b -> Maybe AStore
inferJump store _ (Goto _) = store
inferJump store dest (If e l1 _)
  | l1 == dest = inferAssertion' store e
  | otherwise  = inferAssertion' store (UOp Not e)
inferJump _ _ (Exit _) = undefined

-- glb of store lattice
glbStore :: Maybe AStore -> Maybe AStore -> Maybe AStore
glbStore Nothing _ = Nothing
glbStore _ Nothing = Nothing
glbStore (Just s1) (Just s2) = glbStore' s1 s2

-- glb of strict store
glbStore' :: AStore -> AStore -> Maybe AStore
glbStore' s1 s2 = sequence $ combineWith aglb s1 s2

-- lub in AStore lattice
lubStore :: Maybe AStore -> Maybe AStore -> Maybe AStore
lubStore Nothing s = s
lubStore s Nothing = s
lubStore (Just s1) (Just s2) = return $ combineWith alub s1 s2

-- Infer new store from step and store
inferStep :: AStore -> Step -> Maybe AStore
inferStep s Skip = return s
inferStep s (Assert e) = inferAssertion s e
inferStep s (Update n op e) =
  do let av1 = get n s
     av2 <- inferExpr s e
     av3 <- aRevOp op av1 av2
     return $ set n av3 s
inferStep s (Replacement q1 q2) =
  let (av, s'') = inferConstruct s q2
  in inferDeconstruct s'' q1 av

-- Construct abstract value and store from pattern
inferConstruct :: AStore -> Pattern -> (AValue, AStore)
inferConstruct s (QConst v) = (aValue v, s)
inferConstruct s (QVar n) = (get n s, set n ANil s)
inferConstruct s (QPair q1 q2) =
  let (av1, s'') = inferConstruct s   q1
      (av2, s')  = inferConstruct s'' q2
  in (APair av1 av2, s')

-- Deconstruct the abstract value, fails if impossible
inferDeconstruct :: AStore -> Pattern -> AValue -> Maybe AStore
inferDeconstruct s (QConst v) av =
  do assert $ aValue v `aglb` av /= Nothing
     return s
inferDeconstruct s (QVar n) av =
  do let av1 = get n s
     assert $ ANil `lteStrict` av1
     return $ set n av s
inferDeconstruct s (QPair q1 q2) av =
  do (av1, av2) <-
        case av of
          APair av1' av2' -> return (av1', av2')
          Any -> return (Any, Any)
          ANonNil -> return (Any, Any)
          _ -> Nothing
     s' <- inferDeconstruct s q1 av1
     inferDeconstruct s' q2 av2

-- Infer the abstract value of an expression
inferExpr :: AStore -> Expr -> Maybe AValue
inferExpr _ (Const v) = return $ aValue v
inferExpr s (Var n) = return $ get n s
inferExpr s (Op op e1 e2) =
  do av1 <- inferExpr s e1
     av2 <- inferExpr s e2
     aBinOp op av1 av2
inferExpr s (UOp op e) =
  do av <- inferExpr s e
     aUnOp op av

-- Infer from assertion/transition
inferAssertion :: AStore -> Expr -> Maybe AStore
inferAssertion s (Var n) =
  let av = get n s
  in case av `aglb` ANonNil of
      Just v -> return $ set n v s
      Nothing -> Nothing
inferAssertion s (Op And e1 e2) =
  let ms1 = inferAssertion s e1
      ms2 = inferAssertion s e2
  in glbStore ms1 ms2
inferAssertion s (Op Or e1 e2) =
  let ms1 = inferAssertion s e1
      ms2 = inferAssertion s e2
  in lubStore ms1 ms2
inferAssertion s (UOp Not e) =
  case e of
    (Var n) ->
      let av = get n s
      in case av `aglb` ANil of
          Just v -> return $ set n v s
          Nothing -> Nothing
    (UOp Not e') -> inferAssertion s e'
    (Op And e1 e2) -> inferAssertion s (Op Or  (UOp Not e1) (UOp Not e2))
    (Op Or e1 e2)  -> inferAssertion s (Op And (UOp Not e1) (UOp Not e2))
    _ -> return s
inferAssertion s _ = return s

inferAssertion' :: Maybe AStore -> Expr -> Maybe AStore
inferAssertion' Nothing _ = Nothing
inferAssertion' (Just s) e = inferAssertion s e

inferDecl :: VariableDecl -> Maybe AStore
inferDecl decl =
  let vars = allVars decl
      allAny = fromList $ map (\n -> (n, Any)) vars
      store = sets (nonInput decl) ANil allAny
  in Just store
