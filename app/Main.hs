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

data Mode = Specialize | Invert

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
usage = "Usage: PERevFlow [invert/specialize] [-i FILE.rl] [-o FILE.rl] [-s FILE.spec] [-silent] [-showTrace] [-skipSpec] [-skipPost] [-lift]"

processInput :: [String] -> Opts -> (Opts, [String])
processInput ("-o" : file : ss) opts = processInput ss $ opts { outputFile = file }
processInput ("-i" : file : ss) opts = processInput ss $ opts { inputFile = file }
processInput ("-s" : file : ss) opts = processInput ss $ opts { specFile = file }
processInput ("invert" : ss) opts =  processInput ss $ opts { mode = Invert}
processInput ("specialize" : ss) opts =  processInput ss $ opts { mode = Specialize}
processInput ("-skipSpec" : ss) opts = processInput ss $ opts { skipSpec = True }
processInput ("-showTrace" : ss) opts = processInput ss $ opts { showTrace = True }
processInput ("-skipPost" : ss) opts = processInput ss $ opts { skipPost = True }
processInput ("-verbose" : ss) opts = processInput ss $ opts { verbose = True}
processInput ("-lift" : ss) opts = processInput ss $ opts { liftstate = True}
processInput ss opts = (opts, ss)

trace :: Opts -> String -> IO ()
trace opts s | verbose opts = putStrLn s
             | otherwise = return ()

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
  
-- TODO: Improve SPEC format, Logging improvements
-- TODO: Semantics of decl, check at exit (plumbing :()
main :: IO ()
main =
  do ss <- getArgs
     when (null ss) $ die usage 
     let (opts, ss1) = processInput ss defaultOpts
     unless (null ss1) $ die "Failed to parse whole command input" 
     case mode opts of
      Specialize -> specMain opts
      Invert     -> invMain opts

invMain :: Opts -> IO ()
invMain opts = 
  do trace opts $ "Reading program from " ++ inputFile opts ++ "."
     progStr <- readFile $ inputFile opts
     trace opts "- Parsing program."
     prog <- fromEM "parsing" $ parseProg progStr
     _ <- fromEM "performing wellformedness check of input prog" 
              $ wellformedProg id prog
     trace opts "- Inverting program."
     let invProg = invertProg prog
     _ <- fromEM "performing wellformedness check of reverse prog" 
              $ wellformedProg id invProg
     let str = prettyProg id invProg
     trace opts $ "Writing to " ++ outputFile opts 
     writeFile (outputFile opts) str 
     trace opts "Program ran successfully"


specMain :: Opts -> IO ()
specMain opts = 
  do trace opts $ "Reading program from " ++ inputFile opts ++ "."
     progStr <- readFile $ inputFile opts
     trace opts "- Parsing program."
     prog <- fromEM "parsing" $ parseProg progStr
     let decl = fst prog
     _ <- fromEM "performing wellformedness check of input prog" 
              $ wellformedProg id prog
     trace opts $ "Reading division (and specilization data) from " ++ specFile opts ++ "."
     specStr <- readFile $ specFile opts
     trace opts "- Parsing division."
     initStore <- fromEM "parsing" $ parseSpec specStr
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
     trace opts $ "Writing to " ++ outputFile opts 
     writeFile (outputFile opts) str 
     trace opts "Program ran successfully"

main2 :: Opts -> Program' String -> Store -> IO String
main2 opts prog2 store = 
  do trace opts "- Specializing"
     (res, l) <- fromLEM "specializing" $ specialize id prog2 store "entry"
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
     trace opts "- Cleaning names"
     let numeratedStore = enumerateAnn merged
     let clean = changeLabel (\(lab,s) -> lab ++ "_" ++ show s) numeratedStore
     trace opts "- Merging exits"
     let newName lb ub = "exit_merge_" ++ show lb ++ "_" ++ show ub
     let singleExit = mergeExits newName (decl, clean)
     showLength $ snd singleExit
     return $ prettyProg id singleExit 