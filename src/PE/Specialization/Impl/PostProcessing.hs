module PE.Specialization.Impl.PostProcessing where

import Utils.Maps
import Utils.Error

import RL.AST
import RL.Operators
import RL.Program
import RL.Values

import PE.AST2
import PE.SpecValues

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)

-- Constant folding of an expression
-- Logging included
constFoldE :: Expr -> LEM Expr
constFoldE (Const v) = return $ Const v
constFoldE (Var n)   = return $ Var n
constFoldE (UOp op e) = do
  e' <- constFoldE e
  case e' of
    Const v ->
      logM "U.Op. reduced." >> emToLEM (Const <$> calcU op v)
    _ -> return $ UOp op e'
constFoldE (Op op e1 e2) = do
  e1' <- constFoldE e1
  e2' <- constFoldE e2
  case (op, e1', e2') of
    (_, Const v1, Const v2) ->
      logM "Bin.Op. reduced." >> emToLEM (Const <$> calc op v1 v2)
    (And, Const v, _) ->
      logM "'And' reduced." >> return (if truthy v then e2' else Const falseV)
    (And, _, Const v) ->
      logM "'And' reduced." >> return (if truthy v then e1' else Const falseV)
    (Or, Const v, _) ->
      logM "'Or' reduced." >> return (if truthy v then Const trueV else e2')
    (Or, _, Const v) ->
      logM "'Or' reduced." >> return (if truthy v then Const trueV else e1')
    _ -> return $ Op op e1' e2'

-- Do constant folding in a program, removing blocks that fail assertions
-- Logging included
constFold :: [Block a b] -> LEM [Block a b]
constFold p = concat <$> mapM constFoldB p
  where
    constFoldF (Fi e l1 l2) =
      do e' <- constFoldE e
         return $ case e' of
          Const v -> From $ if truthy v then l1 else l2
          _ -> Fi e' l1 l2
    constFoldF f = return f
    constFoldJ (If e l1 l2) =
      do e' <- constFoldE e
         return $ case e' of
          Const v -> Goto $ if truthy v then l1 else l2
          _ -> If e' l1 l2
    constFoldJ j = return j
    constFoldS (Update n op e) =
      do e' <- constFoldE e; return [Update n op e']
    constFoldS (Assert e) = do
      e' <- constFoldE e
      case e' of
        Const v -> if truthy v
                    then return []
                    else raise (Left "Assert failed")
        _ -> return [Assert e']
    constFoldS step = return [step]
    constFoldB b =
      let lem =
            (do f' <- constFoldF $ from b
                bs' <- mapM constFoldS $ body b
                j' <- constFoldJ $ jump b
                return b{from = f', body = concat bs', jump = j'})
      in case runLEM lem of
        (Right res, l) -> do
          logManyM l >> return [res]
        (Left err, _) ->
          logM ("Error during constant propagation: " ++ err) >> return []


-- Merge all explicator blocks
mergeExplicators :: Ord a => (a -> Int -> Int -> a) -> [Block (Explicated a) SpecStore]
                          -> [Block (Explicated a) SpecStore]
mergeExplicators annotateExpl p =
  let (expl, rest) = L.partition (\b -> case label b of
                                      Regular _ -> False; _ -> True) p
      explLabel b =
        let (l', s) = name b
            (l, ns) = getBoth l'
        in  (l, s `withouts` ns)
      explGroups' = L.groupBy ((==) `on` explLabel) $
                    L.sortBy  (compare `on` explLabel) expl
      originss = map (map name) explGroups'
      origins = map head originss
      dests = map (head . jumpLabels . jump . head) explGroups'
      nss = map (getVars . fst) origins
      explGroups = map (\es -> zipWith (\e i -> (e, i, i)) es [1..] ) explGroups'
      (finals, _, _, _, extras) = L.unzip5 $ zipWith mergeGroup nss explGroups
      corrections = zipWith3 (\d o b -> (d, o, name b)) dests originss finals
  in map (correctBlock corrections) rest ++ finals ++ concat extras
    where
      on f g x y = f (g x) (g y)
      getLabel n = case n of
                       Regular l -> l
                       Explicator l _ -> l
      getVars n = case n of
                       Regular _ -> []
                       Explicator _ ns -> ns
      getBoth n = case n of
                       Regular l -> (l, [])
                       Explicator l ns -> (l, ns)
      getStore = snd . name
      annotateBlock b lb ub =
        let l =  label b
        in Explicator (annotateExpl (getLabel l) lb ub) (getVars l)
      mergeGroup = mergeBlocks annotateBlock getStore
      correctBlock corrections b@Block {name = l, from = k} =
        case L.find (\(l',_, _) -> l == l') corrections of
          Just (_, origs, new) -> b{from = mapFrom (correctFroms origs new) k}
          Nothing -> b
      correctFroms origs new l = if l `elem` origs then new else l

-- Merge all residual exits into a single one and
-- generalize the static output variables that differ between exits
mergeExits :: VariableDecl -> (a -> Int -> Int -> a) -> Program a SpecStore -> (Program a SpecStore, [(Name, SpecValue)])
mergeExits origdecl annotateExit (VariableDecl{input = inp, output = out, temp = tmp}, p) =
  let (exits, rest) = L.partition isExit p
      stores = map (toList . getExitStore) exits
      ns = map fst $ head stores
      vals = map (map snd) stores
      tpls = zip ns $ L.transpose vals
      toFix =  map fst $ filter (\(_,vs) -> any (head vs /=) vs) tpls
      initialized = map (initFix toFix) exits
      explGroups = zipWith (\b i -> (b, i, i)) initialized [1..]
      (exit, _, _, _, extras) = mergeBlocks' toFix explGroups
      newDecl = VariableDecl inp (out ++ toFix) (filter (`notElem` toFix) tmp)
      finalState = filter (\(n,_) -> n `notElem` toFix && n `elem` output origdecl) $ head stores
  in ((newDecl, rest ++ extras ++ [exit] ), finalState)
  where
    getExitStore b =
      case jump b of
        Exit s -> s
        _ -> undefined
    initFix toFix b =
      let getVal n = toVal $ get n (getExitStore b)
          initSteps = map (\n -> Update n Xor (Const (getVal n))) toFix
      in b{body = body b ++ initSteps}
    annotateExit' b = annotateExit $ label b
    mergeBlocks' = mergeBlocks annotateExit' getExitStore

-- Generalized block merging
mergeBlocks :: (Block a SpecStore -> Int -> Int -> a) -> (Block a SpecStore -> SpecStore) -> [Name] -> [(Block a SpecStore, Int, Int)]
                -> (Block a SpecStore, Int, Int, [SpecStore], [Block a SpecStore])
mergeBlocks _ _ _ [] = undefined
mergeBlocks _ getStore _ [(b, lb, ub)] = (b, lb, ub, [getStore b], [])
mergeBlocks annotateLab getStore ns es =
  let len = length es `div` 2
      (b1, lb1, ub1, ss1, bs1) = mergeBlocks annotateLab getStore ns $ take len es
      (b2, lb2, ub2, ss2, bs2) = mergeBlocks annotateLab getStore ns $ drop len es
      (lb, ub) = (min lb1 lb2, max ub1 ub2)
      (b, _, _) =  head es
      l' = annotateLab b lb ub
      newName = (l', emptyMap)
      newJump = Goto newName
      b1' = b1 { jump = newJump}
      b2' = b2 { jump = newJump}
      vals = map (\store -> map (\n -> toVal $ get n store) ns) ss1
      comps = map (zipWith (\n v -> Op Equal (Var n) (Const v)) ns) vals
      conjs = map (foldl1 (Op And)) comps
      expr = foldl1 (Op Or) conjs
      newBlock = Block
        { name = newName
        , from = Fi expr (name b1) (name b2)
        , body = []
        , jump = jump b1
        }
  in (newBlock, lb, ub, ss1++ss2, b1':b2':bs1++bs2)

-- Remove all blocks that can never reach an exit
removeDeadBlocks :: (Eq a, Eq b) => [Block a b] -> [Block a b]
removeDeadBlocks p =
  let bs = filter isExit p
      liveBlocks = traceProg bs []
  in filter (`elem` liveBlocks) p
  where
    traceProg [] res = res
    traceProg (b:bs) res
      | b `elem` res = traceProg bs res
      | otherwise =
        let new = mapMaybe (getBlock p) $ fromLabels $ from b
        in traceProg (new ++ bs) (b:res)

-- Fix malformed jumps and come-froms
changeConditionals :: (Eq a, Eq b) => [Block a b] -> [Block a b]
changeConditionals prog = map changeCond prog
  where
    changeCond b =
      let (f, assFrom) = checkFrom $ from b
          (j, assJump) = checkJump $ jump b
      in b {from = f, jump = j, body = assFrom ++ body b ++ assJump}
    checkFrom f = case f of
      Fi e l1 l2 | not $ l2 `nameIn` prog ->
        (From l1, [Assert e])
      Fi e l1 l2 | not $ l1 `nameIn` prog ->
        (From l2, [Assert (UOp Not e)])
      _ -> (f , [])
    checkJump j = case j of
      If e l1 l2 | not $ l2 `nameIn` prog ->
        (Goto l1, [Assert e])
      If e l1 l2 | not $ l1 `nameIn` prog ->
        (Goto l2, [Assert (UOp Not e)])
      _ -> (j , [])

-- Compress blocks that chain together
compressPaths :: (Ord a, Ord b) => [Block a b] -> [Block a b]
compressPaths p =
  let entry = getEntryName p
      bss = chainBlocks [entry] []
      bs = map combineBlocks bss
      relabels = Map.fromList $ map relabelPair bss
      p' = mapBoth (updateL relabels) bs
  in removeEmptyBlocks p'
  where
    combineBlocks bs = Block {
      name = name $ head bs
    , from = from $ head bs
    , body = concatMap body bs
    , jump = jump $ last bs
    }
    relabelPair bs = (name $ last bs, name $ head bs)
    updateL relab l = fromMaybe l $ Map.lookup l relab
    chainBlocks [] _ = []
    chainBlocks (l:ls) seen
      | l `elem` seen = chainBlocks ls seen
      | otherwise =
        case getBlock p l of
          Just b ->
            let (chain, new) = getChain b
            in chain : chainBlocks (new ++ ls) (l : seen)
          Nothing -> chainBlocks ls seen
    getChain b = case jump b of
      Exit _ -> ([b], [])
      If _ l1 l2 -> ([b], [l1, l2])
      Goto l -> case getBlock p l of
                  Just b'@Block {from = From l'} | name b == l' ->
                      let (bs, ls) = getChain b'
                      in (b:bs, ls)
                  Just _ -> ([b], [l])
                  Nothing -> ([b], [])

-- remove empty blocks that are not necessary
removeEmptyBlocks :: (Ord a, Ord b) => [Block a b] -> [Block a b]
removeEmptyBlocks p =
  let (nonempty, removable) = L.partition isNeeded p
      relabelFroms = Map.fromList $ map (\b ->
                      (name b, head . fromLabels $ from b)) removable
      relabelJumps = Map.fromList $ map (\b ->
                      (name b, head . jumpLabels $ jump b)) removable
      jumpsFixed = map (mapJumpB (updateL relabelJumps)) nonempty
      fromsFixed = map (mapFromB (updateL relabelFroms)) jumpsFixed
  in fromsFixed
  where
    updateL relab l = fromMaybe l $ Map.lookup l relab
    mapJumpB f b = b{jump = mapJump f $ jump b}
    mapFromB f b = b{from = mapFrom f $ from b}
    isNeeded Block{from = f, body = b, jump = j} =
      let semanticFrom = case f of From _ -> False; _ -> True
          semanticJump = case j of Goto _ -> False; _ -> True
      in not (null b) || semanticFrom || semanticJump

-- Transform the store annotations into integers.
enumerateAnn :: Ord b => [Block a b] -> [Block a Int]
enumerateAnn p =
  let stores = L.nub $ map (snd . name) p
      enum s =
        case L.elemIndex s stores of
          Just i -> i+1
          Nothing -> 0
  in mapProgStore enum p
