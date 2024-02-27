module Main (main) where

import Options.Applicative

import Values
import Parser

import BTA
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
import Control.Monad (when, unless)

data Options = Specialize SpecOptions 
             | Invert InvertOptions 
             | Interpret InterpretOptions
             | Bench BenchOptions

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
  , intVerbose   :: Bool
  }

data BenchOptions = BenchOptions
  { benchFile :: String
  , benchSpecFile :: String
  , dynamicVars :: [Name]
  , benchVerbose :: Bool
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
              <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Show messages and info for each phase")
              )

interpretParser :: Parser Options
interpretParser = Interpret <$> (InterpretOptions
               <$> argument str (metavar "<Input RL file>")
               <*> argument str (metavar "<Spec file>")
               <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Show messages and info for each phase")                              
              )

benchParser :: Parser Options
benchParser = Bench <$> (BenchOptions
           <$> argument str (metavar "<Input RL file>")
           <*> argument str (metavar "<Spec file>")
           <*> argument strp (metavar "<List of dynamic input vars>")
           <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Show messages and info for each phase")
           )
  where
    strp = eitherReader $ return . words

optParser :: Parser Options
optParser = hsubparser
              ( command "spec" (info specParser
                (progDesc "Specialize an RL program"))
             <> command "invert" (info inverterParser
                (progDesc "Invert an RL program"))
             <> command "interpret" (info interpretParser
                (progDesc "Interpret an RL program"))
             <> command "bench" (info benchParser
                (progDesc "Run various benchmarks on an RL program"))
              )

optsParser :: ParserInfo Options
optsParser = info (optParser <**> helper)
  ( fullDesc
  <> progDesc "Various operations on RL programs (spec/invert/interpret)"
  )

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

main :: IO ()
main = do
  options <- execParser optsParser
  case options of
    Specialize opts -> specMain  opts
    Invert     opts -> invMain   opts
    Interpret  opts -> intMain   opts
    Bench      opts -> benchMain opts
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
     putStrLn $ prettyStats stats

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
     let nprog = normalize "init" "stop" prog (\l i -> l ++ "_" ++ show i)
     trace v "- Creating initial binding."
     d <- fromEM "binding" $ makeDiv initStore decl
     trace v $ "- Performing BTA " ++ if uniform then "(uniform)" else "(pointwise)"
     let btaFunc = if uniform then btaUniform else btaPW
     let congruentDiv = btaFunc nprog d
     let store = makeSpecStore decl initStore
     trace v "- Annotating program"
     let prog2 = annotateProg congruentDiv nprog
     _ <- fromEM "wellformedness of 2 level lang. This should never happen. Please report." 
                 (wellformedProg' congruentDiv prog2)
     let explicated = explicate congruentDiv prog2 (\l i -> l ++ "_" ++ show i)
     out <- if skipSpecPhase specOpts 
              then do trace v "- Skip specialization";
                      return $ prettyProg' (serializeExpl id) explicated
              else specMain2 specOpts decl explicated store
     writeOutput v outputPath out

btaUniform, btaPW :: NormProgram Label -> Division -> DivisionPW Label
btaUniform = congruentUniformDiv
btaPW p d = 
  let initd = initPWDiv p d
  in makeCongruentPW p initd

specMain2 :: SpecOptions -> VariableDecl -> Program' (Explicated Label) -> Store -> IO String
specMain2 specOpts decl prog2 store = 
  let v = specVerbose specOpts
  in
  do trace v "- Specializing"
     (res, l) <- fromLEM "specializing" $ specialize decl prog2 store (Regular "entry")
     trace (specTrace specOpts) $ "Trace: (label: State)\n" ++ unlines l
     let (resdecl, resbody) = res
     if skipPost specOpts
     then do 
        trace v "- Skip post processing"
        let clean = mapCombine (serializeAnn (serializeExpl id)) resbody 
        return $ prettyProg id (resdecl, clean)
     else do 
        trace v "- POST PROCESSING"
        (prog, staticVals) <- specPostProcess v decl res
        printStaticOutput decl staticVals
        return $ prettyProg id prog

specPostProcess :: Bool -> VariableDecl -> Program (Explicated Label) (Maybe Store)
                -> IO (Program Label (), [(Name, SpecValue)])
specPostProcess v origdecl (decl, prog) = 
  do let showLength p = trace v $ "Nr. of blocks: " ++ show (length p)
     showLength prog
     trace v "- Folding constants"
     (folded, out) <- fromLEM "Folding" $ constFold prog
     trace v $ "Expressions reduced: " ++ show (length out)
     trace v "- Merging explicitors"
     let cleanStores = mapProgStore (fromMaybe emptyStore) folded
     let merged' = mergeExplicators (\l i1 i2 -> l ++ "_e" ++ show i1 ++ "_" ++ show i2) cleanStores
     let merged = mapLabel (serializeExpl id) merged'
     showLength merged
     trace v "- Removing dead blocks"
     let liveProg = removeDeadBlocks merged
     showLength liveProg
     trace v "- Adding assertions / Making blocks wellformed"
     let withAssertions = changeConditionals liveProg
     trace v "- Merging exits"
     let ((decl', singleExit), staticVals) = 
            mergeExits origdecl (\l i1 i2 -> l ++ "_x" ++ show i1 ++ "_" ++ show i2) (decl, withAssertions)
     showLength singleExit
     trace v "- Compressing paths"
     let compressed = compressPaths singleExit
     showLength compressed
     trace v "- Cleaning names"
     let numeratedStore = enumerateAnn compressed
     let clean = mapCombine (\l s -> l ++ "_" ++ show s) numeratedStore
     return ((decl', clean), staticVals)

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

benchMain :: BenchOptions -> IO ()
benchMain BenchOptions { benchFile     = inputPath
                       , benchSpecFile = specPath
                       , dynamicVars   = dyn
                       , benchVerbose  = v } = 
  do prog <- parseFile "program" False parseProg inputPath
     fromEM "wellformedness of input prog" $ wellformedProg prog
     trace v "Initial interpretation"
     completeStore <- parseFile "input" False parseSpec specPath
     ((outInt, statsInt), _) <- fromLEM "execution" $ runProgram prog completeStore
     let decl = fst prog
     let initStore = completeStore `withouts` dyn
     d <- fromEM "binding" $ makeDiv initStore decl
     let specStore = makeSpecStore decl initStore
     let nprog = normalize' prog
     let pipeline' = pipeline  nprog d decl specStore completeStore outInt
     (resUni, statsUni) <- pipeline' "UNI" btaUniform
     (resPW, statsPW) <- pipeline' "PW" btaPW
     putStrLn $ "-- Source program\n" ++ prettyStats statsInt 
     putStrLn $ prettySize prog
     putStrLn $ "-- Uniform PE\n" ++ prettyStats statsUni
     putStrLn $ prettySize resUni
     putStrLn $ "-- Pointwise PE\n" ++ prettyStats statsPW
     putStrLn $ prettySize resPW
  where 
    pipeline nprog d decl specstore runstore origOut mode bta =
      do prog2 <- preprocess mode bta nprog d
         res <- specialize' mode decl prog2 specstore
         (prog, outStatic) <- postprocess mode decl res
         (outRes, stats) <- run mode prog runstore
         let combinedOut = updateWithStore (makeStore outStatic) outRes
         verifyOutput mode origOut combinedOut
         fromEM ("wellformedness of residual prog " ++ mode) $ 
            wellformedProg prog
         return (prog, stats)
    preprocess mode bta nprog d =
      do trace v $ "Preprocessing " ++ mode
         let cdiv = bta nprog d
         let p2 = annotateProg cdiv nprog
         fromEM "wellformedness of RL2 prog" (wellformedProg' cdiv p2)
         let expl = explicate cdiv p2 (\l i -> l ++ "_" ++ show i)
         return expl 
    run mode p s = 
      do trace v $ "Interpreting " ++ mode
         (res, _) <- fromLEM "execution" $ runProgram' p s
         return res
    normalize' p = normalize "init" "stop" p (\l i -> l ++ "_" ++ show i) 
    specialize' mode d p s = 
      do trace v $ "Specializing " ++ mode
         (prog, _) <- fromLEM "specializing" $ specialize d p s (Regular "entry")
         return prog
    postprocess mode origdecl prog = 
      do trace v $ "Postprocessing " ++ mode
         specPostProcess False origdecl prog
    verifyOutput mode regularOut specOut = 
      do trace v $ "Checking output " ++ mode
         unless (all (\(n, val) -> val == find' n specOut) (storeToList regularOut)) 
            $ die "The regular and specialized output do not match"
    prettySize (_, prog) = "Total Blocks: " ++ show (length prog)
                        ++ ", Total Lines: " ++ show (sum $ map (length . body) prog) 

makeSpecStore :: VariableDecl -> Store -> Store
makeSpecStore decl s =
  let dynStore = makeStore . map (\n -> (n, Dynamic)) $ getVarsDecl decl
      nilStore = makeStore . map (\n -> (n, Static Nil)) $ nonInput decl
      store = dynStore `updateWithStore` nilStore `updateWithStore` s
  in store
