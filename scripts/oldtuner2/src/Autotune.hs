{--

 Copyright (c) 2009 Maciej Pacula

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use,
 copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following
 conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.

--}


module Autotune
    (
      Options (..)
    , SizeMode (..)
    , autotune
    ) where
    

import Petabricks
import PetabricksUtils (showCallGraph)

import GP.GrammarParser (parseGrammar)
import GP.Possibly
import GP.Generator
import GP.Types
import GP.Genetic
import GP.Utils

import Data.Char (toLower)
import Data.List (intersperse, sort, isSuffixOf)
import qualified Data.Map as Map
import Debug.Trace
import Text.Printf

import System.Random
import System.IO

import Control.Monad

import System.FilePath
import Directory


-- same as putStrLn, but prints out to stderr
putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr


{----------------------------------------------------------------------
  AUTOTUNER STATE
  
  Data structures that represent the internal state of the autotuner.
  This data structure is passed around the GP system as user data.
----------------------------------------------------------------------}
            

data AutotuneState = AutotuneState {
      tunableInfos                    :: [TunableInfo]
    , rands                           :: [Int]
    , testSize                        :: Int
    , defaultTiming                   :: ExecutionStats
    , program                         :: Program
    , minTiming                       :: ExecutionStats
    , minInputSize                    :: Int
    , maxInputSize                    :: Int
    , timeLimit                       :: Double
    , bestGenerationTime              :: Double
    , bestFraction                    :: Double
    , generation                      :: Int
    , verbose                         :: Bool
    , showProgress                    :: Bool
    , writeOutput                     :: String -> IO ()
    , progressOutput                  :: String -> IO ()
    , benchmarkInterval               :: Int
    , benchmarkSizes                  :: [Int]
    , evaluationCount                 :: Int
    , autotunePopulationSize          :: Int
    , sizeMode                        :: SizeMode
    , timeCutoff                      :: Double
    , dumpPopulation                  :: Bool
    , populationOutput                :: String -> IO ()
    }


data SizeMode = GrowRange | GrowMax | OneSize { oneSize :: Int }
              deriving Show


{----------------------------------------------------------------------
  AUTOTUNER OPTIONS visible to the outside world
----------------------------------------------------------------------}


data Options = Options {
      optBenchmarkName                   :: String
    , optMutationProbability             :: Double
    , optPopulationSize                  :: Int
    , optFractionBest                    :: Double
    , optTimeLimit                       :: Double
    , optProgressOutput                  :: String -> IO ()
    , optWriteOutput                     :: String -> IO ()
    , optVerbose                         :: Bool
    , optShowProgress                    :: Bool
    , optSwapProbability                 :: Double
    , optMaxGenerations                  :: Int
    , optBenchmarkInterval               :: Int
    , optBenchmarkSizes                  :: [Int]
    , optRandomSelectionProbability      :: Double
    , optCompleteMutationProbability     :: Double
    , optSizeMode                        :: SizeMode
    , optTimeCutoff                      :: Double
    , optDumpPopulation                  :: Bool
    , optPopulationOutput                :: String -> IO ()
    , optSelectionMethod                 :: SelectionMethod
    }





{----------------------------------------------------------------------
  GP INTEGRATION

  The code below is responsible for grammar generation and syntax tree evaluation.
  
  The GP evolves SyntaxTrees, which the code below can convert into lists of tunab.les

----------------------------------------------------------------------}


-- builds a grammar that can be fed into the GP system to evolve configuration files
buildGrammar :: [TunableInfo] -> Possibly [Expansion]
buildGrammar tunables = do exs <- parseGrammar $ unlines (rootExpansion : tunableExpansions)
                           return exs
    where
      -- root rule that expands to rules for individual tunables
      rootExpansion = (concat . (intersperse " ") $ map (\s -> "<" ++ s ++ ", ch = median_hook>") names) ++ " :: Root"
      
      tunableExpansions = map (\name -> "[" ++  name ++ "] :: " ++ name) names

      names = map cutoffName tunables


-- the root term that the GP will expand and evolve. Here "Root" expands to all tunables.
startTerm = (NonterminalTerm (PrimitiveType "Root") True "")


-- evaluates a syntax tree to a config file (a list of tunables)
evalTunables :: [TunableInfo] -> SyntaxTree -> [Tunable]
evalTunables infos (Branch _ _ children) = map (evalTunable infos) children


-- converts a syntax tree branch to a tunable
evalTunable :: [TunableInfo] -> SyntaxTree -> Tunable
evalTunable infos (Branch _ (PrimitiveType tunableName) children) =
    SequentialCutoff { tunableInfo = info, cutoffValue = val}
    where
      -- tunable metadata    
      info = head $ filter ((== tunableName) . cutoffName) infos

      -- tunable value
      val :: Int
      val = evalNumber $ head children
            
      -- evaluates a number in a SyntaxTree, converting it to decimal
      evalNumber :: SyntaxTree -> Int
      evalNumber (Leaf str) = read str
evalTunable _ (Branch _ t _) = error $ "Unexpected type: " ++ show t
evalTunable _ (Leaf val) = error $ "Attempted to evaluate a leaf as a tunable: " ++ val





{----------------------------------------------------------------------
  GENERATORS

  Generation of random integers for tunable values. The generators are specified in the
  grammar by enclosing leaf names in square brackets []. For example, "[int]" means that
  the term should expand to a terminal and the value should be generated by the "int" generator.

  All generators are called by the GP system.

----------------------------------------------------------------------}


data IntGeneratorState = IntGeneratorState { intGeneratorChoices :: [Int]
                                           , cutoffInfo          :: TunableInfo
                                           }


intGenerator :: UserGenerator IntGeneratorState
intGenerator = UserGenerator {
                 generate = (\startState@(IntGeneratorState choices info) _ ->
                                 let endState = startState {
                                                  intGeneratorChoices = tail choices
                                                }
                                     choice = head choices
                                     val = if (cutoffMax info - cutoffMin info) > 100
                                           then 2^(scaleToInterval 0 (round $ logBase 2 (fromIntegral (maxBound :: Int))) choice)
                                           else choice
                                     name = cutoffName info
                                 in
                                   (endState, show val)
                            )
                                      
               }


-- creates integer generators used to create configuration files
createGenerators :: Int -> [TunableInfo] -> [(String, UserGenerator IntGeneratorState, IntGeneratorState)]
createGenerators seed ts = fst $ foldr tunableToGenerator ([], randoms (mkStdGen seed)) ts
    where
      tunableToGenerator sci@(SequentialCutoffInfo name _ _ _) (gens, rs) =
          let g = ( name, intGenerator, IntGeneratorState {
                              intGeneratorChoices = randoms (mkStdGen $ head rs)
                            , cutoffInfo = sci
                            }
                  )
              in (g:gens, tail rs)




{----------------------------------------------------------------------

  CROSSOVER HOOKS
  
  Crossover hooks are invoked by the GP system whenever two subtrees of the given type are
  to be crossed over. If a hook exists for the given type, a hook is responsible
  for perfoming the actual crossover operation.

  In our case, we have only one hook for all tunables which either takes the mean
  or swaps values.
  
----------------------------------------------------------------------}


-- the way the grammar is set up, this hook will always get parents that are branches with one
-- child which is a leaf. The value of that leaf is the tunable
medianHook = CrossoverHook { crossoverHook = \randoms (Branch term t [(Leaf val1)]) (Branch _ _ [(Leaf val2)]) ->
                                             let num1 = read val1 :: Int
                                                 num2 = read val2 :: Int
                                                 median = round $ (fromIntegral $ num1 + num2) / 2
                                             in
                                               if head randoms <= 0.5
                                                   then (tail randoms, (Branch term t [(Leaf $ show median)]))
                                                   else (tail randoms, (Branch term t [(Leaf val2)]))
                           }





{----------------------------------------------------------------------
  
  GENERATION MERGERS
  
  After a new generation has been evolved and evaluated, it is compared with the previous one.
  Mergers are responsible for merging previous generation with the new one. They can pick
  only best members from both, discard old population etc etc.

------------------------------------------------------------------------}

-- picks a fixed number of best members from old and new population, and fills
-- remaining spots with the new population
bestSelector :: GenerationMerger AutotuneState
bestSelector state old new = (selectedBest ++ selectedRest, state)
    where
      fraction = bestFraction state
      selectedBest = select fraction all
      selectedRest = take (length new - length selectedBest) $ filter (not . (`elem` selectedBest)) (new ++ old)
      all = (reverse . sort) (old ++ new)
      size = length new
      select x = take (floor ((fromIntegral size) * x))





{----------------------------------------------------------------------
  FITNESS EVALUATION

  Fitness is proportional to speedup
----------------------------------------------------------------------}


fitnessEvaluator :: AutotuneState -> EvaluatedSyntaxTree -> IO (AutotuneState, Double)
fitnessEvaluator state atree =
    do let infos = tunableInfos state
           tunables = normalizeTunables $ buildTrees $ evalTunables infos $ tree atree
           bestTime = bestGenerationTime state
       printProgress state
       result <- timeConfiguration
                 (testSize state)
                 1
                 ((timeCutoff state) * bestTime)
                 tunables
                 (program state)
       case result of
         Right timing ->
             do let speedup = (statsMin (defaultTiming state)) / (statsMin timing)
                    newBestTime = if (statsMin timing) < bestTime
                                  then (statsMin timing)
                                  else bestTime
                    fitness = getFitness speedup (fitnessHistory atree)
                return (state { bestGenerationTime = newBestTime
                              , evaluationCount = evaluationCount state + 1
                              }
                       , fitness)
         Left msg -> do putStrLn $ "Timing run failed when evaluating fitness: " ++ msg
                        return (state, 0)


printProgress :: AutotuneState -> IO ()
printProgress state = do let c = evaluationCount state
                             size = autotunePopulationSize state
                             total = if (generation state) == 1
                                     then size
                                     else 2*size
                             progress = printf "%.2f" ((100 * (fromIntegral c) / (fromIntegral total)) :: Double)
                         
                         let msgTemplate = "% evaluations done"
                             textWidth = (length "    00.00") + (length msgTemplate)
                             msg = padLeft textWidth $ progress ++ msgTemplate
                         if showProgress state
                            then do putStr $ replicate (if c == 1 then 0 else textWidth) '\b'
                                    putStr msg
                            else return ()
                         (progressOutput state) $ msg ++ "\n"
    where
      padLeft :: Int -> String -> String
      padLeft l str = (replicate (l - (length str)) ' ') ++ str
                         
                        


getFitness :: Double -> [Double] -> Double
getFitness speedup hist = sum vals / sum pows
    where vals = map (\ (x, c) -> c*x) (zip (speedup : hist) pows)
          
          pows = map (\pow -> 2**(-pow)) [1..fromIntegral $ (length hist) + 1]





{----------------------------------------------------------------------
  INPUT SIZE ADJUSTMENT 
----------------------------------------------------------------------}


-- searches for an input size that results in execution that's close to the user-defined limit
adjustMaxInputSize :: Int -> Double -> (Int -> IO (Either String ExecutionStats)) -> IO (Int)
adjustMaxInputSize currentMax limit runner = do newMax <- adjustMaxInputSizeStep currentMax runner
                                                if newMax > currentMax
                                                   then adjustMaxInputSize newMax limit runner
                                                   else return currentMax
    where
      -- times the best member of the population on the maxSizeimum input size, and if the time is less than
      -- the user-defined limit, proposes a new greater input size. The new size is 150% the old one
      adjustMaxInputSizeStep :: Int -> (Int -> IO (Either String ExecutionStats)) -> IO (Int)
      adjustMaxInputSizeStep maxSize runner = do timingResult <- runner $ max (maxSize+1) (ceiling (1.1 * (fromIntegral maxSize)))
                                                 case timingResult of
                                                   Left err -> do putErrLn $ "Error while adjusting maxSize input size: " ++ err
                                                                  return maxSize
                                                   Right timing ->
                                                     if statsMin timing < limit
                                                     then return $ max (maxSize+1) (ceiling (1.1 * (fromIntegral maxSize)))
                                                     else return maxSize





{----------------------------------------------------------------------
  REPORTERS
  
  Reporters are called after a new generation has been evolved and evaluated
  on the current input size. The name "reporter" is somewhat misleading
  as they do not only report on the autotuning process but can also adjust 
  parameters such as input size.
----------------------------------------------------------------------}

evoReporter :: EvolutionReporter AutotuneState
evoReporter gen state trees =
    do let currentBestSpeedup = (fitness . bestMember) trees
       serializePopulation state trees
       putStrLn "\n"
       putStrLn $ show gen
                    ++ ": Mean speedup: "
                    ++ (roundShow . averageFitness) trees
                    ++ " x. Best speedup: "
                    ++ roundShow currentBestSpeedup ++ " x"
                    ++ "\n"

       -- get the best configuration file in this generation
       -- best syntax tree -> [evalTunables] -> list of tunables (config file) -> [build trees] \
       -- -> list of tunables with some tunables compressed into decision trees -> \
       -- -> [normalize tunables] -> list of tunables with values adjusted to fit into their
       -- valid ranges
       let tunables = normalizeTunables $ buildTrees $ evalTunables (tunableInfos state)
                      $ (tree . bestMember) trees
           size = fromIntegral $ maxInputSize state
           serializedTunables = serializeTunables tunables

       -- if verbose is set, print out the best configuration file
       -- the format is exactly the same as on disk
       if verbose state
           then putStrLn serializedTunables
           else return ()

       -- write best configuration to the user-specified location (usually a file)
       (writeOutput state) serializedTunables
                
       -- time Jason and this autotuner on the current input size as a rough indication of
       -- how well we're doing
       jasonResults <- timeJason (testSize state) 10 (program state)
       bestResults   <- timeConfiguration (testSize state) 10 (10*timeLimit state)
                        tunables (program state)
       case (jasonResults, bestResults) of
         (Right jasonStats, Right bestStats) ->
             do putStrLn $ "\tSpeedup over Jason: "
                             ++ roundShow ((statsMin jasonStats) / (statsMin bestStats))
                             ++ " x"
         (Left err, _) -> putErrLn $ "Timing of Jason's configuration failed: "  ++ err
         (_, Left err) -> putErrLn $ "Timing of autotuner's configuration failed: "  ++ err
                
       -- Benchmark against Jason on some input sizes
       if generation state `mod` (benchmarkInterval state) == 0
           then do putStrLn "\n\tRuning periodic benchmark..."
                   benchmark state tunables $ map round $
                                 map (*size) [0.2, 0.4, 0.6, 0.8, 1.0]
                   benchmark state tunables (benchmarkSizes state)
                   putStrLn "\n"
           else return ()


       -- keep the upper input bound near a level that keeps us close to the user-defined time limit
       (newMinInputSize, newMaxInputSize) <- case sizeMode state of
         OneSize val -> return (val, val)
         _           -> do putStr "\tAdjusting input size... "
                           size <- adjustMaxInputSize (maxInputSize state)
                                   (timeLimit state)
                                   (\n -> timeConfiguration
                                          n
                                          2
                                          (2 * timeLimit state)
                                          tunables
                                    (program state))
                           let newMinInputSize = case sizeMode state of
                                                   GrowMax   -> size
                                                   GrowRange -> minInputSize state
                           putStrLn $ "min: "
                                        ++ show newMinInputSize
                                        ++ ", max: "
                                        ++ show size
                           return (newMinInputSize, size)
                               



       -- pick a new test input size within the adjusted interval
       let rand = head $ rands state
           newSize = scaleToInterval newMinInputSize newMaxInputSize rand
       putStrLn $ "\tInput size for the next generation: " ++ show newSize

       
       -- we'll be measuring speedup relative to this timing
       defaultStats <- guard (\msg -> do putStrLn msg
                                         putErrLn msg
                                         putStr "Speedups for next generation will be innacurate. "
                                         putStrLn "This will not affect the autotuning process."
                                         return $ ExecutionStats 1 1 1 1 1 1 1 1
                             )
                             (timeDefaultExecution newSize 5 $ program state)

       putStrLn $ "\tDefault configuration time: " ++ (show . statsMin) defaultStats ++ " sec\n"
       return state { rands = tail $ rands state
                    , testSize = newSize
                    , defaultTiming = defaultStats
                    , minInputSize = newMinInputSize
                    , maxInputSize = newMaxInputSize
                    , bestGenerationTime = fromIntegral (maxBound :: Int)
                    , generation = 1 + generation state
                    , evaluationCount = 1
                    }
    where
      roundShow :: Double -> String
      roundShow = printf "%.2f"

      guard ::  (a -> IO (b)) -> IO (Either a b) -> IO (b)
      guard f wrapped = do val <- wrapped
                           case val of
                             Left err    -> f err
                             Right val   -> return val


serializePopulation :: AutotuneState -> [EvaluatedSyntaxTree] -> IO ()
serializePopulation state trees = if dumpPopulation state
                                  then
                                      do let writer = (populationOutput state)
                                             output = unlines $ map serializeTree trees
                                         writer $ "\n==== Generation: " ++ show (generation state) ++ " ====\n\n"
                                         writer output
                                         return ()
                                  else return ()

    where serializeTree :: EvaluatedSyntaxTree -> String
          serializeTree atree = let tunables = normalizeTunables
                                              $ buildTrees
                                              $ evalTunables (tunableInfos state) (tree atree)
                                    size = fromIntegral $ maxInputSize state
                                    serializedTunables = serializeTunables tunables
                                in
                                  "------\nFitness: " ++ show (fitness atree)
                                                         ++ "\n\n"
                                                         ++ serializedTunables
                                                         ++ "------\n\n"




{----------------------------------------------------------------------
  BENCHMARKING against the original autotuner
----------------------------------------------------------------------}


benchmark :: AutotuneState -> [Tunable] -> [Int] -> IO ()
benchmark state tunables ns = do mapM (benchmarkInputSize state tunables) ns
                                 return ()
    where
      benchmarkInputSize :: AutotuneState -> [Tunable] -> Int -> IO (Int, Double, Double, Double)
      benchmarkInputSize state tunables n = do myResults <- timeConfiguration n 10 2000 tunables $ program state
                                               jasonsResults <- timeJason n 10 $ program state

                                               case (jasonsResults, myResults) of
                                                 (Right jasonsTime, Right myTime) ->
                                                      do putStrLn $ "\tn = " ++ show n ++ ", me: " ++ show (statsMin myTime)
                                                                      ++ " sec, jason: "
                                                                      ++ show (statsMin jasonsTime)
                                                                      ++ " sec, speedup: "
                                                                      ++ show (statsMin jasonsTime / statsMin myTime)
                                                         return (n
                                                                , statsMin myTime
                                                                , statsMin jasonsTime
                                                                , (statsMin jasonsTime / statsMin myTime)
                                                                )

                                                 (Left err, _) ->
                                                     do putErrLn $ "\tn ="
                                                                  ++ show n
                                                                  ++ ": timing of Jason's configuration failed: "  ++ err
                                                        return (n, 0, 0, 0)
                                                 (_, Left err) ->
                                                     do putErrLn $ "\tn ="
                                                                  ++ show n
                                                                  ++ ": timing of autotuner's configuration failed: "  ++ err
                                                        return (n, 0, 0, 0)
                                 
                                                               
                                              
                                               
                                   




{----------------------------------------------------------------------
  STANDARD PETABRICKS BENCHMARKS
----------------------------------------------------------------------}

findBenchmark :: String -> IO (Maybe Program)
findBenchmark name = do currentDir <- getCurrentDirectory
                        petabricks <- findPetabricks currentDir
                        case petabricks of
                          Nothing        -> return Nothing
                          Just path      -> do let benchmarkRoot = path </> "examples" </> (map toLower name)
                                               exe <- findExecutable name benchmarkRoot
                                               case exe of
                                                 Just exePath -> do let exeName = (snd $ splitFileName exePath)
                                                                    return $ Just (Program benchmarkRoot exeName)
                                                 Nothing      -> return Nothing
                              


findPetabricks :: String -> IO (Maybe FilePath)
findPetabricks root = do let path = root </> "petabricks"
                         found <- doesDirectoryExist path
                         if found
                            then return $ Just path
                            else case root of
                                   "/"       -> return Nothing
                                   otherwise -> findPetabricks $ takeDirectory root



findExecutable :: String -> FilePath -> IO (Maybe FilePath)
findExecutable exe root = do files <- getDirectoryContents root
                             execs <- filterM (\name -> do let path = root </> name
                                                           (Permissions _ _ x _) <- getPermissions path
                                                           return x
                                              )
                                      files
                             if exe `elem` execs
                                then return $ Just (root </> exe)
                                else
                                    -- try other case combinations
                                    do case filter (\name -> lower name == lower exe) execs of
                                         [x] -> return $ Just (root </> x)
                                         _   -> return Nothing
                                where lower = map toLower

findInfo :: Program -> IO (Maybe FilePath)
findInfo (Program root _) = do files <- getDirectoryContents root
                               case filter ((== ".info") . (map toLower). takeExtension) files of
                                 [x] -> return $ Just (root </> x)
                                 _   -> return Nothing


{----------------------------------------------------------------------
  MAIN AUTOTUNING ROUTINE
----------------------------------------------------------------------}


-- Given an either, returns the wrapped value if the either is a Right.
-- Otherwise prints an error message and quits the program.
expectRight :: (Show a) => String -> IO (Either a b) -> IO (b)
expectRight msg wrapped = do val <- wrapped
                             case val of
                               Right val -> return val
                               Left err  -> error $ msg ++ ": " ++ show err


autotune :: Options -> IO ()
autotune options =
    do Just p <- findBenchmark (optBenchmarkName options)
       cpus <- cpuCount
       putStrLn $ "\nDetecting CPUs... " ++ show cpus
       Just infoFilePath <- findInfo p
       putStr "Parsing info file... "
       infoFile <- readFile infoFilePath
       let info = parseInfo infoFile infoFile
       putStrLn "OK"
       putStr "Getting main name... "
       main <- expectRight "Could not get the main name" $ programMainName p
       putStrLn main
       putStr "Resetting config to default... "
       expectRight "Could not reset configuration to its defaults" $ resetConfig p
       putStrLn "OK"

--       putStrLn "Call graph:"
--       putStrLn $ showCallGraph main info
               
       let allTunableInfos = getHiddenTunables ++ (concat $ map transformTunables info)

       putStr "Generating GP grammar..."
       let (Good gpGrammar) = buildGrammar allTunableInfos
       putStrLn "OK"


       initialInputSize <- case optSizeMode options of
                             OneSize val -> do putStrLn $ "Using input size: " ++ show val
                                               return val
                             _           ->
                                 do putStr "Determining initial input size... "
                                    size <- adjustMaxInputSize 1
                                            (optTimeLimit options)
                                            (\n -> timeDefaultExecution n 2 p)
                                    putStrLn $ show size
                                    return size

       putStr "Initial timing of default configuration... "
       initialTiming <- expectRight "Could not time the default configuration"
                        $ timeDefaultExecution initialInputSize 5 p
       putStrLn $ show (statsMin initialTiming) ++ " sec"

       -- seed the random number generator
       seed <- getStdRandom random :: IO (Int)
       let rs = randoms (mkStdGen seed) :: [Int]
  
       let allTunables = initTunables allTunableInfos
           m = fromIntegral (maxBound :: Int)
           autotuneState = AutotuneState { tunableInfos = allTunableInfos
                                         , rands = randoms (mkStdGen $ rs !! 0)
                                         , testSize = initialInputSize
                                         , program = p
                                         , defaultTiming = initialTiming
                                         , minTiming = ExecutionStats 0 m m m m m 0 0
                                         , minInputSize = initialInputSize
                                         , maxInputSize = initialInputSize
                                         , timeLimit = optTimeLimit options
                                         , bestGenerationTime = fromIntegral (maxBound :: Int)
                                         , bestFraction = optFractionBest options
                                         , generation = 1
                                         , verbose = optVerbose options
                                         , showProgress = optShowProgress options
                                         , writeOutput = optWriteOutput options
                                         , benchmarkInterval = optBenchmarkInterval options
                                         , evaluationCount = 1
                                         , autotunePopulationSize = optPopulationSize options
                                         , progressOutput = optProgressOutput options
                                         , benchmarkSizes = optBenchmarkSizes options
                                         , sizeMode = optSizeMode options
                                         , timeCutoff = optTimeCutoff options
                                         , dumpPopulation = optDumpPopulation options
                                         , populationOutput = optPopulationOutput options
                                         }
       
       -- create initial state for the GP system
       let evoState = Evolver { choices             = randoms (mkStdGen $ rs !! 1)             :: [Int]
                              , probabilities       = randomRs (0.0, 1.0) (mkStdGen $ rs !! 2) :: [Double]
                              , grammar             = gpGrammar

                              -- maxTreeDepth is intended for recursive languages. Our trees for config files are
                              -- only 3 levels deep
                              , maxTreeDepth        = 10
                              , mutationProbability = optMutationProbability options
                              , evaluator           = Evaluator { runEval = fitnessEvaluator }
                              , populationSize      = optPopulationSize options
                              , merger              = bestSelector
                              , stopCondition       = \trees -> False
                              , generationNumber    = 1
                              , userState           = autotuneState
                              , userGenerators      = createGenerators (rs !! 3) allTunableInfos
                              , completeMutationProbability = optCompleteMutationProbability options
                              , randomSelectionProbability = optRandomSelectionProbability options
                              , crossoverHooks      = Map.fromList
                                                      [("median_hook"
                                                       , (medianHook, randomRs (0.0, 1.0)
                                                                        (mkStdGen $ rs !! 4) :: [Double]))]
                              , selectionMethod = optSelectionMethod options
                           }

       putStrLn "Invoking the GP system..."
       startEvolving evoState startTerm (optMaxGenerations options - 1) evoReporter
       return ()
