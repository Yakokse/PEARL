module Main (main) where

import Options.Applicative

import Values
import Parser

import BTA
import Pointwise
import Division
import AST
import AST2
import Normalize
import Explicicator
import Utils
import Annotate
import Specialize
import PostProcessing
import PrettyPrint
import Wellformed
import Inverter
import Execute
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import System.Exit (die)
import Control.Monad (when)

data Options = Specialize SpecOptions 
             | Invert InvertOptions 
             | Interpret InterpretOptions

data SpecOptions = SpecOptions 
  { specInpFile   :: String
  , specOutFile   :: String
  , specFile      :: String
  , uniformBTA    :: Bool
  , skipSpecPhase :: Bool
  , skipPost      :: Bool
  , specVerbose   :: Bool
  , specTrace     :: Bool
  }

data InvertOptions = InvertOptions
  { invInpFile :: String
  , invOutFile :: String
  , invVerbose :: Bool
  }

data InterpretOptions = InterpretOptions
  { intFile      :: String
  , intInputFile :: String
  , intTrace     :: Bool
  , intVerbose   :: Bool
  }

specParser :: Parser Options
specParser = Specialize <$> (SpecOptions
          <$> argument str (metavar "<Input RL file>")
          <*> argument str (metavar "<Output path>")
          <*> argument str (metavar "<Spec file>")
          <*> switch (long "uniformBTA"
                           <> short 'u'
                           <> help "Perform uniform BTA rather than pointwise")
          <*> switch (long "skipSpec"
                           <> short 's'
                           <> help "Stop after BTA, printing the RL2 program")
          <*> switch (long "skipPost"
                           <> short 'p'
                           <> help "Stop before post-processing, printing the initial residual program")
          <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Show messages and info for each phase")
          <*> switch (long "trace"
                           <> short 't'
                           <> help "Show trace of specialization (Not recommended)")
          )

inverterParser :: Parser Options
inverterParser = Invert <$> (InvertOptions
              <$> argument str (metavar "<Input RL file>")
              <*> argument str (metavar "<Output path>")
              <*> option auto (long "verbose"
                               <> short 'v'
                               <> value True
                               <> help "Show messages and info for each phase")
              )

interpretParser :: Parser Options
interpretParser = Interpret <$> (InterpretOptions
               <$> argument str (metavar "<Input RL file>")
               <*> argument str (metavar "<Spec file>")
               <*> option auto (long "trace"
                                <> short 't'
                                <> value True
                                <> help "Show trace of execution")
               <*> option auto (long "verbose"
                                <> short 'v'
                                <> value True
                                <> help "Show messages and info for each phase")                              
              )

optParser :: Parser Options
optParser = hsubparser
              ( command "spec" (info specParser
                (progDesc "Specialize an RL program"))
             <> command "invert" (info inverterParser
                (progDesc "Invert an RL program"))
             <> command "interpret" (info interpretParser
                (progDesc "Interpret an RL program"))
              )

optsParser :: ParserInfo Options
optsParser = info (optParser <**> helper)
  ( fullDesc
  <> progDesc "Various operations on RL programs (spec/invert/interpret)" 
  <> header "hello - a test for optparse-applicative")

trace :: Bool -> String -> IO ()
trace b s = when b $ putStrLn s

fromEM :: String -> EM a -> IO a
fromEM _ (Right a) = return a
fromEM s (Left e) = die $ "Error while " ++ s ++ ": " ++ e

fromLEM :: String -> LEM a -> IO (a, [String])
fromLEM _ (LEM (Right a, l)) = return (a, l)
fromLEM s (LEM (Left e, l)) = 
  do putStrLn "LOG:" 
     mapM_ putStrLn l
     die $ "Error while " ++ s ++ ": " ++ e  

parseFile :: String -> Bool -> (String -> EM a) -> String -> IO a
parseFile goal verbose parse file =
  do trace verbose $ "- Reading " ++ goal ++ " from file: " ++ show file
     content <- readFile file
     trace verbose "- Parsing input"
     fromEM "parsing" $ parse content

writeOutput :: Bool -> String -> String -> IO ()
writeOutput verbose path content =
  do trace verbose $ "Writing to " ++ path 
     writeFile path content 

-- TODO: Improve SPEC format
main :: IO ()
main = do
  options <- execParser optsParser
  case options of
    Specialize opts -> specMain opts
    Invert     opts -> invMain  opts
    Interpret  opts -> intMain  opts
  putStrLn "Program completed succesfully!"

invMain :: InvertOptions -> IO ()
invMain InvertOptions { invInpFile = inputPath
                      , invOutFile = outputPath
                      , invVerbose = v} =
  do prog <- parseFile "program" v parseProg inputPath
     _ <- fromEM "performing wellformedness check of input prog" 
              $ wellformedProg prog
     trace v "- Inverting program."
     let invProg = invertProg prog
     _ <- fromEM "performing wellformedness check of reverse prog" 
              $ wellformedProg invProg
     let out = prettyProg id invProg
     writeOutput v outputPath out 


-- TODO: Execution trace
intMain :: InterpretOptions -> IO ()
intMain InterpretOptions { intFile = filePath
                         , intInputFile = inputPath
                         , intVerbose = v} =
  do prog <- parseFile "program" v parseProg filePath
     _ <- fromEM "performing wellformedness check of input prog" 
              $ wellformedProg prog
     initStore <- parseFile "division and specilization data" v parseSpec inputPath
     (out, _) <- fromLEM "execution" $ runProgram prog initStore
     let (outStore, stats) = out
     trace v "Output store: "
     let outvals = filter (\(n, _) -> n `elem` output (fst prog)) 
                    $ storeToList outStore
     let outstr = map (\(n, val) -> n ++ ": " ++ prettyBTVal val) outvals
     putStrLn (unlines outstr)
     trace v "Execution statistics: "
     print stats -- TODO: Pretty print this

specMain :: SpecOptions -> IO ()
specMain specOpts@SpecOptions { specInpFile = inputPath 
                              , specOutFile = outputPath
                              , uniformBTA  = uniform
                              , specFile    = specPath
                              , specVerbose = v} = 
  do prog <- parseFile "program" v parseProg inputPath
     let decl = fst prog
     _ <- fromEM "performing wellformedness check of input prog" 
              $ wellformedProg prog
     initStore <- parseFile "division and specilization data" 
                    v parseSpec specPath
     trace v "- Normalizing input program."
     let nprog = normalize "init" prog (\l i -> l ++ "_" ++ show i)
     trace v "- Creating initial binding."
     d <- fromEM "binding" $ makeDiv initStore decl
     --trace v $ prettyDiv d
     trace v $ "- Performing BTA " ++ if uniform then "(uniform)" else "(pointwise)"
     let btaFunc = if uniform then btaUniform else pwUniform
     let congruentDiv = btaFunc nprog d
     -- trace v $ prettyDiv congruentDiv
     let dynStore = makeStore . map (\n -> (n, Dynamic)) $ getVarsDecl decl
     let nilStore = makeStore . map (\n -> (n, Static Nil)) $ nonInput decl
     let store = dynStore `updateWithStore` nilStore `updateWithStore` initStore
     trace v "- Annotating program"
     let prog2 = annotateProg congruentDiv nprog
     let explicated = explicate prog2 (\l i -> l ++ "_" ++ show i)
     _ <- fromEM "wellformedness of 2 level lang. This should never happen. Please report." 
                 (wellformedProg' congruentDiv prog2)
     out <- if skipSpecPhase specOpts 
              then do trace v "- Skip specialization";
                      return $ prettyProg' (serializeExpl id) explicated
              else specMain2 specOpts decl explicated store
     writeOutput v outputPath out

btaUniform, pwUniform :: NormProgram Label -> Division -> DivisionPW Label
btaUniform p d = 
  let ud = makeCongruent p d
  in congruentUniformDiv p ud

pwUniform p d = 
  let initd = initPWDiv p d
  in makeCongruentPW p initd

-- todo: pipeline cleanup
specMain2 :: SpecOptions -> VariableDecl -> Program' (Explicated Label) -> Store -> IO String
specMain2 specOpts decl prog2 store = 
  let v = specVerbose specOpts
  in
  do trace v "- Specializing"
     (res, l) <- fromLEM "specializing" $ specialize decl prog2 store (Regular "entry")
     trace (specTrace specOpts) $ "Trace: (label: State)\n" ++ unlines l
     let (resdecl, lifted) = res
     if skipPost specOpts
      then do trace v "- Skip post processing"
              let clean = mapCombine (serializeAnn (serializeExpl id)) lifted 
              return $ prettyProg id (resdecl, clean)
      else specmain3 specOpts decl (resdecl, lifted)

specmain3 :: SpecOptions -> VariableDecl -> Program (Explicated Label) (Maybe Store) -> IO String
specmain3 specOpts origdecl prog' = 
  let v = specVerbose specOpts
  in
  do let (decl, prog) = prog'
     trace v "- POST PROCESSING"
     let showLength p = trace v $ "Nr. of blocks: " ++ show (length p)
     showLength prog
     trace v "- Folding constants"
     (folded, out) <- fromLEM "Folding" $ constFold prog
     trace v $ "Expressions reduced: " ++ show (length out)
     trace v "- Removing dead blocks"
     let liveProg = removeDeadBlocks folded
     showLength liveProg
     trace v "- Adding assertions / Making blocks wellformed"
     let withAssertions = changeConditionals liveProg
     let cleanStores = mapProgStore (fromMaybe emptyStore) withAssertions
     trace v "- Merging explicitors"
     let merged' = mergeExplicators (\l i -> l ++ "_" ++ show i) cleanStores
     let merged = mapLabel (serializeExpl id) merged'
     showLength merged
     trace v "- Merging exits"
     let ((decl', singleExit), staticVals) = 
            mergeExits origdecl (\l i -> l ++ "_" ++ show i) (decl, merged)
     showLength singleExit
     trace v "- Compressing paths"
     compressed <- fromEM "Path compression" $ compressPaths singleExit
     showLength compressed
     trace v "- Cleaning names"
     let numeratedStore = enumerateAnn compressed
     let clean = mapCombine (\l s -> l ++ "_" ++ show s) numeratedStore
     printStaticOutput origdecl staticVals
     return $ prettyProg id (decl', clean)

printStaticOutput :: VariableDecl -> [(Name, SpecValue)] -> IO ()
printStaticOutput decl tpls =
  do trace True "-- FINAL STATIC OUTPUT --"
     let outvals = filter (\(n, v) -> n `elem` output decl && v /= Dynamic) tpls
     let strings = map (\(n,v) -> n ++ " = " ++ showSpecVal v) outvals
     putStrLn $ intercalate "\n" strings
     trace True "-------------------------"
  where 
    showSpecVal (Static v) = prettyVal v
    showSpecVal Dynamic = undefined
