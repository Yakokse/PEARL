module PostProcessing where
import AST
import AST2 
import Utils
import qualified Data.List as L
import qualified Data.Map.Strict as Map

import Values
import Operators
import Data.Maybe (fromMaybe, mapMaybe)

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


mergeExplicators :: Ord a => (a -> Int -> Int -> a) -> [Block (Explicated a) Store] -> [Block (Explicated a) Store] 
mergeExplicators annotateExpl p = 
  let (expl, rest) = L.partition (\b -> case fst $ name b of 
                                      Regular _ -> False; _ -> True) p
      explLabel = getLabel . fst . name
      explGroups' = L.groupBy (\b1 b2 -> explLabel b1 == explLabel b2) $ 
                    L.sortBy (\b1 b2 -> explLabel b1 `compare` explLabel b2) expl
      origins = map (name . head) explGroups'
      dests = map (head . jumpLabels . jump . head) explGroups'
      nss = map (getVars . fst) origins
      explGroups = map (\es -> zipWith (\e i -> (e, i, i)) es [1..] ) explGroups'
      (finals, _, _, _, extras) = L.unzip5 $ zipWith mergeGroup nss explGroups
      corrections = zipWith3 (\d o b -> (d, o, name b)) dests origins finals
  in map (correctBlock corrections) rest ++ finals ++ concat extras
    where 
      getLabel n = case n of 
                       Regular l -> l 
                       Explicator l _ -> l
      getVars n = case n of 
                       Regular _ -> []
                       Explicator _ ns -> ns
      getStore = snd . name
      annotateBlock b lb ub = 
        let l =  label b
        in Explicator (annotateExpl (getLabel l) lb ub) (getVars l)
      mergeGroup = mergeBlocks annotateBlock getStore
      correctBlock corrections b@Block {name = l, jump = j} =
        case L.find (\(l',_, _) -> l == l') corrections of 
          Just (_, orig, new) -> b{jump = mapJump (correctJump orig new) j}
          Nothing -> b
      correctJump orig new l = if l == orig then new else l    

mergeExits :: VariableDecl -> (a -> Int -> Int -> a) -> Program a Store -> (Program a Store, [(Name, SpecValue)])
mergeExits origdecl annotateExit (VariableDecl{input = inp, output = out, temp = tmp}, p) = 
  let (exits, rest) = L.partition isExit p
      stores = map (storeToList . getExitStore) exits
      -- TODO: one tmp is static nil, other is dynamic?
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
      let getVal = findErr (getExitStore b)
          initSteps = map (\n -> Update n Xor (Const (getVal n))) toFix
      in b{body = body b ++ initSteps}
    annotateExit' b = annotateExit $ label b
    mergeBlocks' = mergeBlocks annotateExit' getExitStore

mergeBlocks :: (Block a Store -> Int -> Int -> a) -> (Block a Store -> Store) -> [Name] -> [(Block a Store, Int, Int)] 
                -> (Block a Store, Int, Int, [Store], [Block a Store])
mergeBlocks _ _ _ [] = undefined
mergeBlocks _ getStore _ [(b, lb, ub)] = (b, lb, ub, [getStore b], [])
mergeBlocks annotateLab getStore ns es = 
  let len = length es `div` 2
      (b1, lb1, ub1, ss1, bs1) = mergeBlocks annotateLab getStore ns $ take len es
      (b2, lb2, ub2, ss2, bs2) = mergeBlocks annotateLab getStore ns $ drop len es
      (lb, ub) = (min lb1 lb2, max ub1 ub2)
      (b, _, _) =  head es
      l' = annotateLab b lb ub
      newName = (l', emptyStore)
      newJump = Goto newName
      b1' = b1 { jump = newJump}
      b2' = b2 { jump = newJump}
      vals = map (\store -> map (findErr store) ns) ss1
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

compressPaths :: (Ord a, Ord b) => [Block a b] -> EM [Block a b]
compressPaths p = 
  do entry <- getEntryBlock p
     let bss = chainBlocks [name entry] []
     let bs = map combineBlocks bss
     let relabels = Map.fromList $ map relabelPair bss
     let p' = mapBoth (updateL relabels) bs
     return p'
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

enumerateAnn :: Ord b => [Block a b] -> [Block a Int]
enumerateAnn p = 
  let stores = L.nub $ map (snd . name) p
      enum s = 
        case L.elemIndex s stores of
          Just i -> i+1
          Nothing -> 0
  in mapProgStore enum p
