module Main (main) where

import AST
import Parser ( parseProg )
import System.Exit (die)
import System.Environment (getArgs)

--main :: IO ()
--main = case parseProg "test" of
--        Left s -> putStrLn "Error"
--        Right s -> putStrLn "Yeah"

defaultPath :: String
defaultPath = "output.rl"

usage :: String
usage = "Usage: PERevFlow [-o FILE.rl] FILE.rl"

processOpts :: [String] -> String -> (String, [String])
processOpts ("-o" : file : ss) _ = processOpts ss file
processOpts ss o = (o, ss)


fromEM :: String -> EM a -> IO a
fromEM _ (Right a) = return a
fromEM s (Left e) = die $ "Error while " ++ s ++ ": " ++ e

main :: IO ()
main =
  do ss <- getArgs
     let (outputFile, ss1) = processOpts ss defaultPath
     if length ss1 == 1 then return () else die usage
     s <- readFile (head ss1)
     prog <- fromEM "parsing" $ parseProg s
     putStrLn $ "From " ++ head ss1 
     putStrLn $ "Writing to " ++ outputFile 
     writeFile outputFile "Test"