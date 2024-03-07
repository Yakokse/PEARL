module Preprocessing.Impl.Explicicator where

import Preprocessing.AST2
import Utils
import Values
import Preprocessing.Division
import Data.Maybe (catMaybes)
import Maps
import Specialization.SpecValues

-- add explicators for a given RL2 program
explicate :: Ord a => PWDivision a -> Program' a -> (a -> Int -> a) -> Program' (Explicated a)
explicate pwd p f =
  let (renames', blocks') = unzip $ map (explicateBlock pwd p f) p
      (renames, blocks) = (concat renames', concat blocks')
  in fixComeFroms renames blocks

-- fix come-froms for blocks where an explicator was inserted before it
fixComeFroms :: Ord a => [(Explicated a, (Explicated a, Explicated a))] -> [Block' (Explicated a)]
                     -> Program' (Explicated a)
fixComeFroms [] bs = bs
fixComeFroms ((l, (target, replace)) : ls) bs =
  let bs' = map (\b -> if name' b == l then fixBlock b else b) bs
  in fixComeFroms ls bs'
  where
    fixBlock b@Block'{from' = k} = b{from' = fixFrom k}
    fixLabel l' = if l' == target then replace else l'
    fixFrom Entry' = Entry'
    fixFrom (From' l') = From' (fixLabel l')
    fixFrom (Fi' e t l1 l2) = Fi' e t (fixLabel l1) (fixLabel l2)

-- "Explicate" a single block
-- returning the block along with possible explicators
-- + some extra information for relabelling
explicateBlock :: Ord a => PWDivision a -> Program' a -> (a -> Int -> a) -> Block' a
                       -> ([(Explicated a, (Explicated a, Explicated a))], [Block' (Explicated a)])
explicateBlock pwd p f b@Block'{name' = src, jump' = j} =
  let (_, d2) = get src pwd
      b' = mapBlock' Regular b
      pending = toBeExplicated pwd d2 j
      withDest = combineJumpWith (,) j pending
      explicators = mapJumpInt (createExplicator p f src) withDest
      explicatorBlocks = catMaybes . jumpLabels' $ mapJump' snd explicators
      jNew = mapJump' (uncurry renameOrigJump) explicators
      bNew = b'{jump' = jNew}
      renames = map renameInfo explicatorBlocks
  in (renames, bNew : explicatorBlocks)
  where
    renameOrigJump l Nothing = Regular l
    renameOrigJump _ (Just expl) = name' expl
    renameInfo x = (head . jumpLabels' $ jump' x, (head . fromLabels' $ from' x, name' x))

-- For a given jump, give the list of variables that need to be generalized
-- in each label
toBeExplicated :: Ord a => PWDivision a -> Division -> Jump' a -> Jump' [Name]
toBeExplicated pwd d j =
  let nextdivs = mapJump' (\l -> toList . fst $ get l pwd ) j
      combined = mapJump' (zipWith
                            (\(n1, l1) (n2, l2) ->
                              if n1 /= n2
                              then error "Non-matching vars in divs"
                              else (n1, l1, l2))
                            (toList d)) nextdivs
      notMatching = mapJump' (filter (\(_, l1, l2) -> l1 /= l2)) combined
      explicators = mapJump' (map (\(n, l1, l2) ->
                                    case (l1, l2) of
                                      (BTStatic, BTDynamic) -> n
                                      _ -> error "BTA failed monotonicity"
                            )) notMatching
  in explicators

-- create an explicator block for generalizing variables
-- integer used to distinguish between branches
createExplicator :: Eq a => Program' a -> (a -> Int -> a) -> a -> (a, [Name]) -> Int
                         -> (a, Maybe (Block' (Explicated a)))
createExplicator _ _ _ (dest, []) _ = (dest, Nothing)
createExplicator p f src (dest, ns) idx = (dest, return (Block'
  { name' = Explicator (f src idx) ns
  , initDiv = unexplicate . initDiv $ getBlockUnsafe' p dest
  , from' = From' $ Regular src
  , body' = map Generalize ns
  , jump' = Goto' $ Regular dest
  }))
  where
    unexplicate = sets ns (repeat BTStatic)

-- annotate jump labels and distinguish between paths
mapJumpInt :: (a -> Int -> b) -> Jump' a -> Jump' b
mapJumpInt _ Exit' = Exit'
mapJumpInt f (Goto' l) = Goto' (f l 1)
mapJumpInt f (If' l e l1 l2) = If' l e (f l1 1) (f l2 2)

-- combine labels of two jumps with a function
combineJumpWith :: (a -> b -> c) -> Jump' a -> Jump' b -> Jump' c
combineJumpWith _ Exit' Exit' = Exit'
combineJumpWith f (Goto' l1) (Goto' l2) = Goto' (f l1 l2)
combineJumpWith f (If' l e l1 l3) (If' _ _ l2 l4) = If' l e (f l1 l2) (f l3 l4)
combineJumpWith _ _ _ = error "Mismatching jumps in union."
