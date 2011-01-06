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

module Main ( main ) where


import Autotune
import GP.Genetic ( SelectionMethod (..) )
import System
import System.IO
import System.Console.GetOpt
import Control.Exception
import Debug.Trace
import Data.Char

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case getOpt RequireOrder options args of
    (actions, [],      [])   -> do opts <- foldl (>>=) (return defaultOptions) actions
                                   hSetBuffering stdout NoBuffering
                                   printConfiguration opts
                                   autotune opts
                                   return ()
    (_,       nonOpts, [])   -> error $ "Unrecognized arguments: " ++ unwords nonOpts
    (_,        _,      msgs) -> error $ concat msgs ++ usageInfo (header progName) options
                  
  return ()

header prog = "Usage: " ++ prog ++ " [OPTION...]"

      
printConfiguration :: Options -> IO ()
printConfiguration opts = do putStrLn "CONFIGURATION\n"
                             printString "benchmark"                 $ optBenchmarkName opts
                             printOption "generations"               $ optMaxGenerations opts
                             printOption "benchmark interval"        $ optBenchmarkInterval opts
                             printOption "P(mutation)"               $ optMutationProbability opts
                             printOption "P(regenerate | mutation)"  $ optCompleteMutationProbability opts
                             printOption "P(random selection)"       $ optRandomSelectionProbability opts
                             printOption "selection method"          $ optSelectionMethod opts
                             printOption "population size"           $ optPopulationSize opts
                             printOption "best fraction"             $ optFractionBest opts
                             printString "time limit"                $ show (optTimeLimit opts) ++ " sec"
                             printOption "P(swap)"                   $ optSwapProbability opts
                             printOption "verbose"                   $ optVerbose opts
                             printOption "size mode"                 $ optSizeMode opts
                             printOption "benchmark sizes"           $ optBenchmarkSizes opts
                             printOption "time cutoff"               $ optTimeCutoff opts
                             putStrLn ""
                             putStrLn ""


printOption :: Show a => String -> a -> IO ()
printOption msg = (printString msg) . show

printString :: String -> String -> IO ()
printString left right = putStrLn $ "\t" ++ left ++ ": " ++ padding ++ right
    where
      width = 30
      padding = replicate (width - length left) ' '
  

options = [ Option ['v'] ["version"] (NoArg showVersion) "show version number"

          , Option ['b'] ["benchmark"] (ReqArg readBenchmarkName "NAME") "benchmark name"

          , Option ['g'] ["generations"] (ReqArg readMaxGenerations "NUMBER") "number of generations"

          , Option ['i'] ["interval"] (ReqArg readBenchmarkInterval "INTERVAL") "number of generations between benchmarks"

          , Option [] ["benchmark-sizes"] (ReqArg readBenchmarkSizes "s1, s2, ...")
                       "input sizes to benchmark"

          , Option [] ["size-mode"] (ReqArg readSizeMode "grow-range|grow-max|number") "input size choice mode"

          , Option ['m'] ["mutationpr"] (ReqArg readMutationProbability "PROBABILITY")
                       "mutation probability of each new offspring (0-1)"

          , Option ['c'] ["completepr"] (ReqArg readCompleteMutationProbability "PROBABILITY")
                       "complete mutation probability"

          , Option ['r'] ["randompr"] (ReqArg readRandomSelectionProbability "PROBABILITY")
                       "probability of selecting a parent at random, not according to fitness"

          , Option ['p'] ["population"] (ReqArg readPopulationSize "SIZE") "population size (1+)"

          , Option ['f'] ["best"] (ReqArg readFractionBest "FRACTION") $ "fraction of best configuration files from combined " ++
                       "old and new population that will make it to the next generation. The rest will be selected at random " ++
                       "from newly generated configuration files"

          , Option ['l'] ["limit"] (ReqArg readTimeLimit "TIME") "time limit for each evaluation (seconds)"

          , Option ['o'] ["output"] (ReqArg readOutputFile "FILE") $ "output file that best configuration file will " ++
                       "be saved to for each generation"

          , Option [] ["progress-file"] (ReqArg readProgressFile "FILE") "file to which progress stats will be written"

          , Option [] ["population-dump"] (ReqArg readPopulationDump "FILE") "file to which populations will be written"

          , Option [] ["time-cutoff"] (ReqArg readTimeCutoff "CUTOFF") "cutoff time for evaluation relative to best individual"

          , Option [] ["tournament-selection"] (ReqArg readSelectionMethod "POOL-SIZE") "use tournament instead of roulette selection"

          , Option [] ["verbose"] (NoArg readVerbose) "turns on additional output"

          , Option [] ["show-progress"] (NoArg readProgress) "shows % progress"

          , Option ['s'] ["swappr"] (ReqArg readSwapProbability "PROBABILITY")
                       $ "probability of swapping tunables on crossover vs. " ++
                       "taking the median"
          ]

readBenchmarkName arg opt = return opt { optBenchmarkName = arg }
readMutationProbability arg opt = return opt { optMutationProbability = readFraction arg }
readCompleteMutationProbability arg opt = return opt { optCompleteMutationProbability = readFraction arg }
readRandomSelectionProbability arg opt = return opt { optRandomSelectionProbability = readFraction arg }
readMaxGenerations arg opt = return opt { optMaxGenerations = let val = read arg
                                                              in
                                                                if val < 1 then error $ "Invalid number of generations: " ++ arg
                                                                else val
                                        }
readBenchmarkSizes arg opt = let val = read $ "[" ++ arg ++ "]"
                             in
                               return opt { optBenchmarkSizes = val }

readBenchmarkInterval arg opt =
    return opt { optBenchmarkInterval = let val = read arg
                                        in
                                          if val < 1 then error $ "Invalid benchmark interval: " ++ arg
                                          else val
               }

readTimeCutoff arg opt = return opt { optTimeCutoff = read arg }

readSizeMode arg opt = let mode = if all id $ map isDigit arg
                                  then (OneSize $ read arg)
                                  else case arg of
                                         "grow-range"  -> GrowRange
                                         "grow-max"    -> GrowMax
                                         _             -> error $ "Unknown size mode: " ++ arg
                       in
                       return opt { optSizeMode = mode }


readSelectionMethod arg opt =
    let method = if all id $ map isDigit arg
                 then (Tournament (read arg))
                 else (error ("Expected a number but got: " ++ arg))
    in
      return opt { optSelectionMethod = method }


readPopulationSize arg opt = return opt { optPopulationSize = let val = read arg
                                                           in
                                                             if val < 1 then error $ "Invalid population size: " ++ arg
                                                             else val
                                        }
readFractionBest arg opt = return opt { optFractionBest = read arg }
readTimeLimit arg opt = let val = read arg :: Double                        in
                          if val < 0
                          then error $ "Invalid fraction: " ++ arg
                          else return opt { optTimeLimit = val }
readOutputFile arg opt = return opt { optWriteOutput = writeFile arg }
readProgressFile arg opt = return opt { optProgressOutput = writeFile arg }
readPopulationDump arg opt = return opt { optDumpPopulation = True
                                        , optPopulationOutput = appendFile arg }
readVerbose opt = return opt { optVerbose = True }
readProgress opt = return opt { optShowProgress = True }
readSwapProbability arg opt = return opt { optSwapProbability = readFraction arg }
                                 

readFraction :: String -> Double
readFraction arg = let val = read arg :: Double
                   in
                     if val >= 0 && val <= 1
                     then val
                     else error $ "Invalid probability value: " ++ arg

showVersion _ = do
  putStrLn "GA Autotune for PetaBricks version 0.01a"
  exitWith ExitSuccess


defaultOptions :: Options
defaultOptions = Options {
                   optBenchmarkName = "sort"
                 , optMaxGenerations = 100
                 , optMutationProbability = 0.5
                 , optPopulationSize = 100
                 , optFractionBest = 0.2
                 , optTimeLimit = 0.1
                 , optWriteOutput = writeFile "/dev/null"
                 , optVerbose = False
                 , optShowProgress = False
                 , optSwapProbability = 0.5
                 , optBenchmarkInterval = 10
                 , optRandomSelectionProbability = 0.5
                 , optCompleteMutationProbability = 0.5
                 , optProgressOutput = writeFile "/dev/null"
                 , optBenchmarkSizes = []
                 , optSizeMode = GrowRange
                 , optTimeCutoff = 5
                 , optDumpPopulation = False
                 , optPopulationOutput = writeFile "/dev/null"
                 , optSelectionMethod = Roulette
                 }
