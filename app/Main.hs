module Main (main) where

import Values
import Parser
import System.Exit (die)
import System.Environment (getArgs)
import BTA
import Division
import Utils
import Annotate
import Specialize
import PrettyPrint

data Opts = Opts { outputFile :: String, skipSpec :: Bool, showtrace :: Bool }

defaultOpts :: Opts
defaultOpts = Opts { outputFile = "output.rl", skipSpec = False, showtrace = True}

usage :: String
usage = "Usage: PERevFlow [-o FILE.rl] [-skipSpec] [-silent] PROGRAM.rl PROGRAM.spec"
-- Pipeline: Parse all input -> BTA -> Annotate -> Spec -> Print
-- Todo: Add values
processInput :: [String] -> Opts -> (Opts, [String])
processInput ("-o" : file : ss) opts = processInput ss $ opts {outputFile = file}
processInput ("-skipSpec" : ss) opts = processInput ss $ opts {skipSpec = True}
processInput ("-silent" : ss) opts = processInput ss $ opts { showtrace = False}
processInput ss opts = (opts, ss)

trace :: Bool -> String -> IO ()
trace False _ = return ()
trace True s = putStrLn s

fromEM :: String -> EM a -> IO a
fromEM _ (Right a) = return a
fromEM s (Left e) = die $ "Error while " ++ s ++ ": " ++ e

main :: IO ()
main =
  do ss <- getArgs
     let (opts, ss1) = processInput ss defaultOpts
     if length ss1 == 2 then return () else die usage
     let (progPath, specPath) = (head ss1, ss1 !! 1)
     let logProgress = trace $ showtrace opts
     logProgress $ "Reading program from " ++ head ss1 ++ "."
     progStr <- readFile progPath
     logProgress "Parsing program."
     prog <- fromEM "parsing" $ parseProg progStr
     logProgress $ "Reading division (and specilization data) from " ++ specPath ++ "."
     specStr <- readFile specPath
     logProgress "Parsing division."
     initStore <- fromEM "parsing" $ parseSpec specStr
     let initDiv = makeDiv (vars initStore) (getVarsProg prog) -- initstore
     let cdiv = congruentDiv prog initDiv
     let store = remove (allDyn cdiv) initStore -- initstore
     let prog2 = annotateProg cdiv prog
     str <- if skipSpec opts 
              then return $ prettyProg' id prog2 
              else fromEM "specializing" $ do
                res <- specialize prog2 store "entry"
                let pp = prettyAnn id 
                return $ prettyProg pp res 
     -- todo: verify prog2/entry prog     
     logProgress $ "Writing to " ++ outputFile opts 
     writeFile (outputFile opts) str 
     logProgress "Program ran successfully"