module Main (main) where

import Values
import Parser ( parseProg )
import System.Exit (die)
import System.Environment (getArgs)
import Division
import Utils

defaultPath :: String
defaultPath = "output.rl"

defaultVars :: [Name]
defaultVars = []

usage :: String
usage = "Usage: PERevFlow [-o FILE.rl] [-s VARIABLE VALUE]* FILE.rl"
-- Pipeline: Parse all input -> BTA -> Annotate -> Spec -> Print
-- Todo: Add values
processInput :: [String] -> String -> [Name] -> (String, [Name], [String])
processInput ("-o" : file : ss) _ ns = processInput ss file ns
processInput ("-s" : var : val : ss) out ns = 
  let ns' = var : ns in processInput ss out ns'
processInput ss out d = (out, d, ss)


fromEM :: String -> EM a -> IO a
fromEM _ (Right a) = return a
fromEM s (Left e) = die $ "Error while " ++ s ++ ": " ++ e

main :: IO ()
main =
  do ss <- getArgs
     let (outputFile, d, ss1) = processInput ss defaultPath defaultVars
     if length ss1 <= 1 then return () else die usage
     s <- readFile (head ss1)
     prog <- fromEM "parsing" $ parseProg s
     let div = makeDiv d $ getVarsProg prog
     putStrLn $ "From " ++ head ss1 
     putStrLn $ "Writing to " ++ outputFile 
     writeFile outputFile "Test"