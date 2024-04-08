module Assertions.Impl.Bidirectional where

import Utils.Maps

import RL.AST
import RL.Program
import RL.Variables

import Assertions.Impl.Abstraction
import Assertions.Impl.Analysis
import Assertions.Impl.Removal

import Inversion.Inverter

import qualified Data.List as List
import Control.Monad (foldM)
import Control.Applicative (Applicative(liftA2))

type State2 a b = Map (a,b) (Maybe (AStore, AStore))

-- Detect and remove redundat assertions in a given program
removeAssertions2 :: (Ord a, Ord b) => Program a b -> Program a b
removeAssertions2 (decl, p) =
  let final = fixpoint initS
      result = removeAssertionsProg2 final (decl, p)
  in result
  where
    entry = getEntryName p
    (declInv, pInv) = invertProg (decl, p)
    exit = getEntryName pInv
    initS = initState (decl, p)
    fixpoint state =
      let state1   = inferProg2 state (decl, p) [entry]
          state2   = inferProg2 (invertState state) (declInv, pInv) [exit]
          newState = combineWith glbState state1 (invertState state2)
      in if state == newState then newState else fixpoint newState

-- Invert a given state for backwards inference
invertState :: State2 a b -> State2 a b
invertState = mmap . const $ fmap (\(a, b) -> (b,a))

glbState :: Maybe (AStore, AStore) -> Maybe (AStore, AStore) -> Maybe (AStore, AStore)
glbState Nothing _ = Nothing
glbState _ Nothing = Nothing
glbState (Just (s1, s2)) (Just (s1', s2')) =
  do ms1 <- glbStore' s1 s1'
     ms2 <- glbStore' s2 s2'
     return (ms1, ms2)

-- Fix-point iteration for state
inferProg2 :: (Ord a, Ord b) => State2 a b -> Program a b
                             -> [(a,b)] -> State2 a b
inferProg2 state _ [] = state
inferProg2 state prog (l:ls) =
  let b = getBlockUnsafe (snd prog) l
      (state', pending) = inferBlock2 state prog b
      ls' = List.union ls pending
  in inferProg2 state' prog ls'

-- Update state and return new pending points
inferBlock2 :: (Ord a, Ord b) => State2 a b -> Program a b -> Block a b
                              -> (State2 a b, [(a, b)])
inferBlock2 state prog
            Block{name = n, from = f, body = b, jump = j} =
    let mOrigs = get n state
        origS1 = fst <$> mOrigs
        origS2 = snd <$> mOrigs
        postState = mmap (const $ fmap snd) state
        parents = case f of
                  Entry _ -> origS1
                  _ -> inferTransition postState prog n f
        initStore = lubStore Nothing parents
        bSafe = filter (\s -> case s of Assert _ -> False; _ -> True)  b
        outStore = case initStore of
                    Just s -> foldM inferStep s bSafe
                    Nothing -> Nothing
        pending = if origS2 == outStore
                    then []
                    else jumpLabels j
        resultPair = liftA2 (,) initStore outStore
        newState = set n resultPair state
    in (newState, pending)

-- Initial state for analysis
-- non-Input/non-output variables in entry/exit are nil while input/output
-- are any. Remaining points are Bottom
initState :: (Ord a, Ord b) => Program a b -> State2 a b
initState (decl, p) =
  let entry = getEntryName p
      exit = getExitName p
      bottomState = fromList $ map (\b -> (name b, Nothing)) p
  in if entry /= exit
    then set exit finalElem $
         set entry initElem bottomState
    else set entry singleElem bottomState
  where
    vars = allVars decl
    allAny = fromList $ map (\n -> (n, Any)) vars
    initStore = sets (nonInput decl) ANil allAny
    finalStore = sets (nonOutput decl) ANil allAny
    initElem = return (initStore, emptyMap)
    finalElem = return (emptyMap, finalStore)
    singleElem = return (initStore, finalStore)

removeAssertionsProg2 :: (Ord a, Ord b) => State2 a b -> Program a b
                                        -> Program a b
removeAssertionsProg2 state (decl, pbody) =
  let startStore b = fst <$> get (name b) state
      startStores = map startStore pbody
      cleanBody = zipWith removeAssertionsBlock startStores pbody
  in (decl, cleanBody)
