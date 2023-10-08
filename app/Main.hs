module Main (main) where

import Values
import Parser
import System.Exit (die)
import System.Environment (getArgs)
import Control.Monad(when, unless)
import BTA
import Division
import AST
import Utils
import Annotate
import Specialize
import PostProcessing
import PrettyPrint
import Wellformed
import Inverter
import Execute

data Mode = Specialize | Invert | Execute

data Opts = Opts { outputFile :: String
                 , inputFile :: String
                 , specFile :: String
                 , mode :: Mode
                 , skipSpec :: Bool
                 , showTrace :: Bool
                 , verbose :: Bool
                 , skipPost :: Bool
                 , liftstate :: Bool }

defaultOpts :: Opts
defaultOpts = Opts { outputFile = "output.rl"
                   , inputFile = "input.rl"
                   , specFile = "input.spec"
                   , mode = Specialize
                   , skipSpec = False
                   , showTrace = False
                   , verbose = True
                   , skipPost = False
                   , liftstate = False}

usage :: String
usage = "Usage: PERevFlow [invert/specialize] [-i FILE.rl] [-o FILE.rl] [-s FILE.spec] [-silent] [-trace] [-skipSpec] [-skipPost] [-lift]"

processInput :: [String] -> Opts -> (Opts, [String])
processInput ("invert"     : ss) opts = processInput ss $ opts { mode = Invert }
processInput ("specialize" : ss) opts = processInput ss $ opts { mode = Specialize }
processInput ("execute"    : ss) opts = processInput ss $ opts { mode = Execute }
processInput ("-o" : file  : ss) opts = processInput ss $ opts { outputFile = file }
processInput ("-i" : file  : ss) opts = processInput ss $ opts { inputFile = file }
processInput ("-s" : file  : ss) opts = processInput ss $ opts { specFile = file }
processInput ("-skipSpec"  : ss) opts = processInput ss $ opts { skipSpec = True }
processInput ("-trace"     : ss) opts = processInput ss $ opts { showTrace = True }
processInput ("-skipPost"  : ss) opts = processInput ss $ opts { skipPost = True }
processInput ("-verbose"   : ss) opts = processInput ss $ opts { verbose = True }
processInput ("-lift"      : ss) opts = processInput ss $ opts { liftstate = True}
processInput ss opts = (opts, ss)

trace :: Opts -> String -> IO ()
trace opts = traceB (verbose opts)

traceB :: Bool -> String -> IO ()
traceB b s = when b $ putStrLn s

fromEM :: String -> EM a -> IO a
fromEM _ (Right a) = return a
fromEM s (Left e) = die $ "Error while " ++ s ++ ": " ++ e

fromLEM :: String -> LEM a -> IO (a, [String])
fromLEM _ (LEM (Right a, l)) = return (a, l)
fromLEM s (LEM (Left e, l)) = 
  do putStrLn "LOG:" 
     mapM_ putStrLn l
     die $ "Error while " ++ s ++ ": " ++ e  

parseFile :: String -> Opts -> (String -> EM a) -> String -> IO a
parseFile goal opts parse file =
  do trace opts $ "- Reading " ++ goal ++ " from file: " ++ show file
     str <- readFile file
     trace opts "- Parsing input"
     fromEM "parsing" $ parse str

writeOutput :: Opts -> String -> IO ()
writeOutput opts str =
  do trace opts $ "Writing to " ++ outputFile opts 
     writeFile (outputFile opts) str 

-- TODO: Improve SPEC format, Logging improvements, TM BTA policy for output, conditional lift of store (VS TM)
main :: IO ()
main =
  do ss <- getArgs
     when (null ss) $ die usage 
     let (opts, ss1) = processInput ss defaultOpts
     unless (null ss1) $ die "Failed to parse whole command input" 
     case mode opts of
      Specialize -> specMain opts
      Invert     -> invMain opts
      Execute    -> execMain opts
     trace opts "Program ran successfully"

invMain :: Opts -> IO ()
invMain opts = 
  do prog <- parseFile "program" opts parseProg (inputFile opts)
     _ <- fromEM "performing wellformedness check of input prog" 
              $ wellformedProg prog
     trace opts "- Inverting program."
     let invProg = invertProg prog
     _ <- fromEM "performing wellformedness check of reverse prog" 
              $ wellformedProg invProg
     let str = prettyProg id invProg
     writeOutput opts str 

execMain :: Opts -> IO ()
execMain opts =
  do prog <- parseFile "program" opts parseProg (inputFile opts)
     _ <- fromEM "performing wellformedness check of input prog" 
              $ wellformedProg prog
     initStore <- parseFile "division and specilization data" 
                    opts parseSpec (specFile opts)
     (out, _) <- fromLEM "execution" $ runProgram prog initStore
     let (outStore, stats) = out
     trace opts "Output store: "
     let outvals = filter (\(n, _) -> n `elem` output (fst prog)) 
                    $ storeToList outStore
     let outstr = map (\(n,v) -> n ++ ": " ++ prettyVal v) outvals
     putStrLn (unlines outstr)
     trace opts "Execution statistics: "
     putStrLn $ show stats

specMain :: Opts -> IO ()
specMain opts = 
  do prog <- parseFile "program" opts parseProg (inputFile opts)
     let decl = fst prog
     _ <- fromEM "performing wellformedness check of input prog" 
              $ wellformedProg prog
     initStore <- parseFile "division and specilization data" 
                    opts parseSpec (specFile opts)
     trace opts "- Creating initial binding."
     initDiv <- fromEM "binding" $ makeDiv initStore decl
     trace opts $ prettyDiv initDiv
     trace opts "- Performing BTA"
     let congruentDiv = makeCongruent prog initDiv
     trace opts $ prettyDiv congruentDiv
     let nilStore = makeStore . map (\n -> (n, Nil)) $ nonInput decl
     let store = nilStore `updateWithStore` initStore
     trace opts "- Annotating program"
     let prog2 = annotateProg congruentDiv prog
     _ <- fromEM "wellformedness of 2 level lang. This should never happen. Please report." 
                 (wellformedProg' congruentDiv prog2)
     str <- if skipSpec opts 
              then do trace opts "- Skip specialization";
                      return $ prettyProg' id prog2 
              else main2 opts prog2 store
     writeOutput opts str 

main2 :: Opts -> Program' String -> Store -> IO String
main2 opts prog2 store = 
  do trace opts "- Specializing"
     (res, l) <- fromLEM "specializing" $ specialize prog2 store "entry"
     traceB (showTrace opts) $ "Trace: (label: State)\n" ++ unlines l
     let (decl, lifted) = if liftstate opts then liftStore res else res -- TODO: Refine
     let clean = changeLabel (serializeAnn id) lifted
     if skipPost opts
      then do trace opts "- Skip post processing"
              return $ prettyProg id (decl, clean)
      else main3 opts (decl, lifted)

main3 :: Opts -> Program (Annotated String) -> IO String
main3 opts prog' = 
  do let (decl, prog) = prog'
     trace opts "- POST PROCESSING"
     let showLength p = trace opts $ "Nr. of blocks: " ++ show (length p)
     showLength prog
     trace opts "- Folding constants"
     (folded, l) <- fromLEM "Folding" $ constFold prog
     trace opts $ "Expressions reduced: " ++ show (length l)
     trace opts "- Removing dead blocks"
     let liveProg = removeDeadBlocks folded
     showLength liveProg
     trace opts "- Adding assertions"
     let withAssertions = changeConditionals liveProg
     trace opts "- Compressing paths"
     merged <- fromEM "Path compression" $ compressPaths withAssertions
     showLength merged
     let multiExit = exitCount merged > 1
     when multiExit $ trace opts "- Merging exits"
     let newName lb ub = "exit_merge_" ++ show lb ++ "_" ++ show ub
     let (singleDecl, singleProg) = 
          if multiExit 
            then mergeExits newName (decl, merged)
            else (decl, merged)
     when multiExit . showLength $ singleProg
     trace opts "- Cleaning names"
     let numeratedStore = enumerateAnn singleProg
     let clean = changeLabel (\(lab,s) -> lab ++ "_" ++ show s) numeratedStore
     return $ prettyProg id (singleDecl, clean)