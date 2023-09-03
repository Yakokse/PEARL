module PostProcessing where
import AST
import Utils
import Data.List (partition)
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

mergeExits :: Program Name -> Program Name
mergeExits p = 
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
    merge :: [(Block String, IntType, IntType)] -> (Block String, IntType, IntType, [Block String])
    merge [] = undefined
    merge [(b, lb, ub)] = (b, lb, ub, [])
    merge tpls =
      let n = length tpls `div` 2
          (b1, lb1, ub1, bs1) = merge $ take n tpls
          (b2, lb2, ub2, bs2) = merge $ drop n tpls
          (lb, ub) = (min lb1 lb2, max ub1 ub2)
          newName = "exit_merge_" ++ show lb ++ "_" ++ show ub
          newJump = Goto newName
          b1' = b1 { jump = newJump}
          b2' = b2 { jump = newJump}
          expr = Op Less (Var exitVar) (Const lb2)
          newBlock = Block 
            { name = newName
            , from = FromCond expr (name b1) (name b2)
            , body = []
            , jump = Exit
            }
      in (newBlock, lb, ub, b1':b2':bs1++bs2)

    

    