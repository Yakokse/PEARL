module Main (main) where

import Values
import Parser
import System.Exit (die)
import System.Environment (getArgs)
import BTA
import Division
import AST
import Utils
import Annotate
import Specialize
import PostProcessing
import PrettyPrint

data Opts = Opts { outputFile :: String, skipSpec :: Bool, verbose :: Bool, skipPost :: Bool, liftstate :: Bool }

defaultOpts :: Opts
defaultOpts = Opts { outputFile = "output.rl", skipSpec = False, verbose = False, skipPost = False, liftstate = False}

usage :: String
usage = "Usage: PERevFlow [-o FILE.rl] [-skipSpec] [-skipPost] [-verbose] [-liftstate] PROGRAM.rl PROGRAM.spec"

processInput :: [String] -> Opts -> (Opts, [String])
processInput ("-o" : file : ss) opts = processInput ss $ opts { outputFile = file }
processInput ("-skipSpec" : ss) opts = processInput ss $ opts { skipSpec = True }
processInput ("-skipPost" : ss) opts = processInput ss $ opts { skipPost = True }
processInput ("-verbose" : ss) opts = processInput ss $ opts { verbose = True}
processInput ("-liftstate" : ss) opts = processInput ss $ opts { liftstate = True}
processInput ss opts = (opts, ss)

trace :: Opts -> String -> IO ()
trace opts s | verbose opts = putStrLn s
             | otherwise = return ()

fromEM :: String -> EM a -> IO a
fromEM _ (Right a) = return a
fromEM s (Left e) = die $ "Error while " ++ s ++ ": " ++ e

fromLEM :: String -> LEM a -> IO (a, String)
fromLEM _ (LEM (Right a, l)) = return (a, unlines l)
fromLEM s (LEM (Left e, l)) = 
  do putStrLn "LOG:" 
     mapM_ putStrLn l
     die $ "Error while " ++ s ++ ": " ++ e  
  
-- TODO: Verify correctness of initial program, redo spec input maybe? Logging improvements
main :: IO ()
main =
  do ss <- getArgs
     let (opts, ss1) = processInput ss defaultOpts
     if length ss1 == 2 then return () else die usage
     let (progPath, specPath) = (head ss1, ss1 !! 1)
     trace opts $ "Reading program from " ++ head ss1 ++ "."
     progStr <- readFile progPath
     trace opts "- Parsing program."
     prog <- fromEM "parsing" $ parseProg progStr
     trace opts $ "Reading division (and specilization data) from " ++ specPath ++ "."
     specStr <- readFile specPath
     trace opts "- Parsing division."
     initStore <- fromEM "parsing" $ parseSpec specStr
     let initDiv = makeDiv (vars initStore) (getVarsProg prog)
     trace opts $ "Initial bindings:\n================\n" ++ prettyDiv initDiv
     trace opts "- Performing BTA"
     let congruentDiv = makeCongruent prog initDiv
     trace opts $ "After BTA:\n================\n" ++ prettyDiv congruentDiv
     let store = remove (allDyn congruentDiv) initStore
     trace opts "- Annotating program"
     let prog2 = annotateProg congruentDiv prog
     str <- if skipSpec opts 
              then do trace opts "- Skip specialization";
                      return $ prettyProg' id prog2 
              else main2 opts prog2 store
     -- TODO: verify prog2/entry prog     
     trace opts $ "Writing to " ++ outputFile opts 
     writeFile (outputFile opts) str 
     trace opts "Program ran successfully"

main2 :: Opts -> Program' String -> Store -> IO String
main2 opts prog2 store = 
  do trace opts "- Specializing"
     (res, l) <- fromLEM "specializing" $ specialize id prog2 store "entry"
     trace opts $ "Trace: (label: State)\n" ++ l
     let lifted = if liftstate opts then liftStore res else res
     let clean = changeLabel (serializeAnn id) lifted
     if skipPost opts
      then do trace opts "- Skip post processing"
              return $ prettyProg id clean
      else main3 opts clean

main3 :: Opts -> Program String -> IO String
main3 opts clean = 
  do trace opts "- POST PROCESSING"
     let showLength p = trace opts $ "Nr. of blocks: " ++ show (length p)
     showLength clean
     trace opts "- Merging exits"
     let newName lb ub = "exit_merge_" ++ show lb ++ "_" ++ show ub
     let singleExit = mergeExits newName clean
     showLength singleExit
     trace opts "- Removing dead blocks"
     let liveProg = removeDeadBlocks singleExit
     showLength liveProg
     trace opts "- Adding assertions"
     let withAssertions = changeConditionals liveProg
     trace opts "- Compressing paths"
     merged <- fromEM "Path compression" $ compressPaths withAssertions
     showLength merged
     return $ prettyProg id merged 