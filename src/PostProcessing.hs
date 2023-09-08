module PostProcessing where
import AST
import Utils
import Data.List (partition)
import qualified Data.Map.Strict as Map

import Values

-- TODO: Arbitrary push expressions will remove magic variable
-- Pushnz only?
liftStore :: Program (Annotated a) -> Program (Annotated a)
liftStore = map appendStore 
  where 
    appendStore b 
      | not $ isExit b = b
      | otherwise = b {
          body = body b ++ inline (getStore $ name b)
      }
    inline = foldr (\(n,v) acc -> encode n v ++ acc) [] . storeToList
    encode n (ScalarVal i) = [UpdateV n Add (Const i)]
    encode n (ArrVal a) = 
      map (\(idx, val) -> UpdateA n (Const idx) Add (Const val)) 
        $ arrToList a
    encode n (StackVal a) = 
      let freeVar = "temp_stack_var_" ++ n in 
      concatMap 
        (\i -> [UpdateV freeVar Add (Const i), Push freeVar n]) 
        . reverse $ stackToList a

mergeExits :: (IntType -> IntType -> a) -> Program a -> Program a
mergeExits showBounds p = 
  let (exits, remaining) = partition isExit p in
  case length exits of 
    n | n <= 1 -> p
    _ ->
      let (exit, _, _, newBlocks) = 
            merge $ zipWith (\b i -> (addControl b i,i,i)) exits [0..]
      in remaining ++ newBlocks ++ [exit]
  where
    exitVar = "exit_control_var"
    addControl block val = block {
      body = body block ++ [UpdateV exitVar Add (Const val)]
    }
    merge [] = undefined
    merge [(b, lb, ub)] = (b, lb, ub, [])
    merge tpls =
      let n = length tpls `div` 2
          (b1, lb1, ub1, bs1) = merge $ take n tpls
          (b2, lb2, ub2, bs2) = merge $ drop n tpls
          (lb, ub) = (min lb1 lb2, max ub1 ub2)
          -- newName = "exit_merge_" ++ show lb ++ "_" ++ show ub
          newName = showBounds lb ub
          newJump = Goto newName
          b1' = b1 { jump = newJump}
          b2' = b2 { jump = newJump}
          divider = if ub1 < lb2 then lb2 else lb1
          expr = Op Less (Var exitVar) (Const divider)
          newBlock = Block 
            { name = newName
            , from = FromCond expr (name b1) (name b2)
            , body = []
            , jump = Exit
            }
      in (newBlock, lb, ub, b1':b2':bs1++bs2)    

compressPaths :: Ord a => Program a -> EM (Program a)
compressPaths p = 
  do entry <- getEntryBlock p
     let bss = chainBlocks [name entry] []
     let bs = map combineBlocks bss
     let relabels = Map.fromList $ map relabelPair bss
     let p' = changeLabel (updateL relabels) bs
     return p'
     --let (p', labelMap) = compress [(entry, name entry)] [] Map.empty
     --return $ changeLabel (labelMap Map.!) p'
  where 
    combineBlocks bs = Block {
      name = name $ head bs
    , from = from $ head bs
    , body = concatMap body bs
    , jump = jump $ last bs
    }
    relabelPair bs = (name $ last bs, name $ head bs)
    updateL relab l = 
      case Map.lookup l relab of
        Just l' -> l'
        Nothing -> l
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