module PostProcessing where
import AST
import Utils
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Values
import Operators
import Data.Maybe (maybeToList, fromMaybe)

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

constFold :: [Block a] -> LEM [Block a]
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

liftStore :: Program (Annotated a) -> Program (Annotated a)
liftStore (decl, p) = (decl, map appendStore p)
  where 
    appendStore b 
      | not $ isExit b = b
      | otherwise = b {
          body = body b ++ inline (getStore $ name b)
      }
    inline = foldr (\(n,v) acc -> Update n Xor (Const v)   : acc) [] . storeToList

removeDeadBlocks :: Eq a => [Block a] -> [Block a]
removeDeadBlocks p = 
  let bs = filter isExit p
      liveBlocks = traceProg bs []
  in filter (`elem` liveBlocks) p
  where 
    traceProg [] res = res
    traceProg (b:bs) res 
      | b `elem` res = traceProg bs res
      | otherwise = 
        let new = concatMap (maybeToList . getBlock p) $ fromLabels b
        in traceProg (new ++ bs) (b:res)

changeConditionals :: Eq a => [Block a] -> [Block a]
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

compressPaths :: Ord a => [Block a] -> EM [Block a]
compressPaths p = 
  do entry <- getEntryBlock p
     let bss = chainBlocks [name entry] []
     let bs = map combineBlocks bss
     let relabels = Map.fromList $ map relabelPair bss
     let p' = changeLabel (updateL relabels) bs
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
      Exit -> ([b], [])
      If _ l1 l2 -> ([b], [l1, l2])
      Goto l -> case getBlock p l of
                  Just b'@Block {from = From l'} | name b == l' -> 
                      let (bs, ls) = getChain b'
                      in (b:bs, ls)
                  Just _ -> ([b], [l])
                  Nothing -> ([b], [])

enumerateAnn :: [Block (Annotated a)] -> [Block (a, Int)]
enumerateAnn p = 
  let stores = Set.toList . Set.fromList $ map (snd . name) p
      enum (l, s) = 
        case elemIndex s stores of
          Just i -> (l, i+1)
          Nothing -> (l, 0)
  in changeLabel enum p
