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


module Petabricks
    (
     TransformInfo(TransformInfo)
    , transformName
    , transformTunables
    , transformCallees
    , CalleeInfo(..)
    , parseInfo
    , getHiddenTunables
    , transformByName
    , Program(..)
    , programMainName
    , initTunables
    , serializeTunables
    , timeExecution
    , timeDefaultExecution
    , timeConfiguration
    , timeJason
    , cpuCount
    , TunableInfo(..)
    , Tunable(..)
    , ExecutionStats(..)
    , buildTrees
    , normalizeTunables
    , resetConfig
    ) where


import System.Environment (getEnv)
import System.IO (withFile, IOMode(..))
import System.Process
import System.Exit
import Debug.Trace
import Text.ParserCombinators.Parsec
import Data.Either (rights)
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Data.List (isInfixOf, isSuffixOf, partition, sort, sortBy, groupBy, nub)
import qualified Data.Map as Map
import GP.Utils


-- information about the PetaBricks environment: root directory etc.
data PetaBricksInfo = PetaBricksInfo { rootPath :: String }

petaBricksSrc :: PetaBricksInfo -> String
petaBricksSrc = (++ "/src") . rootPath

petaBricksPbc :: PetaBricksInfo -> String
petaBricksPbc = (++ "/pbc") . petaBricksSrc


-- represents CPU info as returned by /proc/cpuinfo
data CPU = CPU { identifier :: Int }
         deriving (Show)


-- parses a single whitespace character
whitespace :: Parser Char
whitespace = space <|> tab <|> newline


-- skips 0+ whitespace characters
skipWhitespace :: Parser ()
skipWhitespace = skipMany whitespace


-- parses input of the form "processor : <integer>"
parseCPU :: Parser CPU
parseCPU = do skipWhitespace
              string "processor"
              skipMany (whitespace <|> char ':')
              numberStr <- many1 (noneOf "\n")
              let id = read numberStr :: Int
              return $ CPU id


-- given a string in /proc/cpuinfo's format, parses information about all
-- physical and virtual CPUs
parseCPUInfo :: String -> String -> [CPU]
parseCPUInfo name input = rights parses
    where ls = lines input
          parses = map (parse parseCPU name) ls


-- gets the number of CPUs on the local machine
cpuCount :: IO (Int)
cpuCount = do contents <- readFile path
              return $ length (parseCPUInfo path contents)
           where path = "/proc/cpuinfo"



data CompilationResult = Success { stdout :: String, stderr :: String  }
                       | Error   { stdout :: String, stderr :: String }
                         deriving (Show)


-- calls the PetaBricks compiler on the given file
pbc :: PetaBricksInfo -> String -> IO (CompilationResult)
pbc info file = do (exitCode, stdout, stderr) <- readProcessWithExitCode (petaBricksPbc info) [file] ""
                   case exitCode of
                       ExitSuccess   -> return $ Success stdout stderr
                       ExitFailure _ -> return $ Error stdout stderr



{--
  Info files
--}


data TunableInfo = SequentialCutoffInfo { cutoffName :: String
                                        , cutoffInitial :: Int
                                        , cutoffMin :: Int
                                        , cutoffMax :: Int
                                        }
                   deriving (Show, Eq, Ord)


data CalleeInfo = CalleeInfo { calleeName :: String
                             }
                  deriving (Show)


data TransformInfo = TransformInfo { transformName :: String
                                   , transformTunables :: [TunableInfo]
                                   , transformCallees :: [CalleeInfo]
                                   }
                     deriving (Show)

instance Eq TransformInfo where
    (TransformInfo n1 _ _) == (TransformInfo n2 _ _) = n1 == n2



transformByName :: String -> [TransformInfo] -> TransformInfo
transformByName name ts = head $ filter ((== name) . transformName) ts


transform = tag "traininginfo" /> tag "transform"


getContent :: Document -> Content
getContent (Document _ _ e _) = CElem e

-- parses a PetaBricks .info file into a list of TransformInfos
parseInfo :: String -> String -> [TransformInfo]
parseInfo name xmlString = freshParse
    where
      freshParse = map getTransformInfo $ filterTransformInfos doc
                   
      doc = (getContent . xmlParse name) xmlString

      filterTransformInfos :: CFilter
      filterTransformInfos = transform


getTransformInfo :: Content -> TransformInfo
getTransformInfo doc = TransformInfo name (nub tunables) callees
    where
      name = getattr "name" doc
      callees = map getCalleeInfo $ filterCalleeInfos doc
      tunables = map getTunable $ filterTunables doc
      filterCalleeInfos = keep /> tag "calls"
      filterTunables = keep /> tag "tunable"


getattr :: String -> Content -> String
getattr name doc = verbatim (showattr name doc)

getCalleeInfo :: Content -> CalleeInfo
getCalleeInfo doc = CalleeInfo (getattr "callee" doc)


getTunable :: Content -> TunableInfo
getTunable doc = SequentialCutoffInfo name init min max
    where
      name = getattr "name" doc
      init = read $ getattr "initial" doc :: Int
      min = read $ getattr "min" doc :: Int
      max = read $ getattr "max" doc :: Int


-- returns a list of hidden tunables that do not appear in .info files but which are supported by Petabricks
getHiddenTunables :: [TunableInfo]
getHiddenTunables = [SequentialCutoffInfo "worker_threads" 1 1 16]



{--
  Tunables and config file handling
--}

initTunables :: [TunableInfo] -> [Tunable]
initTunables infos = map (\info -> SequentialCutoff info (cutoffInitial info)) infos


data Tunable = SequentialCutoff { tunableInfo :: TunableInfo
                                , cutoffValue :: Int
                                }

             | TreeCutoff       { cutoffTree :: CutoffTree
                                }


data CutoffTree = CutoffRule   { cutoffRule      :: Tunable  }

                | CutoffBranch { branchCutoff    :: Tunable
                               , greaterThanRule :: CutoffTree
                               , lessThanBranch  :: CutoffTree
                               }


isTree :: Tunable -> Bool
isTree (SequentialCutoff info _) = "_lvl" `isInfixOf` cutoffName info

treeName :: Tunable -> String
treeName = (takeWhile (/= '_')) . cutoffName . tunableInfo


detectTrees :: [Tunable] -> ([Tunable], [Tunable])
detectTrees tunables = (nontrees, sortedTrees)
    where
      (trees, nontrees) = partition isTree tunables
      sortedTrees = sortBy (\a b -> compare (cutoffName $ tunableInfo a) (cutoffName $ tunableInfo b)) trees


groupByName :: [Tunable] -> [[Tunable]]
groupByName = groupBy (\a b -> (treeName a) == (treeName b))


-- separates sequential cutoffs from rules
-- [cutoffs & rules] -> ([cutoff], [rule])
separateRules :: [Tunable] -> ([Tunable], [Tunable])
separateRules = partition (\t -> "_cutoff" `isSuffixOf` (cutoffName $ tunableInfo t)) . reverse

-- given tunable name such as "Sortsubarray_0_lvl9_cutoff" returns the level, e.g. 9

getLevel :: Tunable -> Int
getLevel t = case parse parseLevel "level" (cutoffName $ tunableInfo t) of
               Right l  -> l
               Left msg -> error $ show msg

parseLevel :: Parser Int
parseLevel = do manyTill (letter <|> char '_' <|> digit) (try (string "_lvl"))
                level <- many1 digit
                return $ read level


buildTree :: ([Tunable], [Tunable]) -> CutoffTree
buildTree ([], [lastRule]) = CutoffRule lastRule
buildTree (c:cs, r:rs) = CutoffBranch c (CutoffRule r) (buildTree (cs, rs))
buildTree x = error $ "error: " ++ (show x)

buildTrees :: [Tunable] -> [Tunable]
buildTrees tunables = nontrees ++ builtTrees
    where
      (nontrees, trees) = detectTrees tunables

      byName = groupByName trees
      
      separated = map separateRules byName

      builtTrees = map (\(xs, ys) -> TreeCutoff $ buildTree (sortByLevel xs, sortByLevel ys)) separated
      sortByLevel = sortBy (\a b -> compare (getLevel b) (getLevel a))


-- shifts and scales all cutoffs to be in valid ranges
normalizeTunables :: [Tunable] -> [Tunable]
normalizeTunables = map norm
    where
      norm x@(SequentialCutoff (SequentialCutoffInfo _ _ min max) val) = x { cutoffValue = scaleToInterval min max (abs val) }
      norm (TreeCutoff tree) = TreeCutoff $ normalizeTree tree


-- scales and shifts cutoffs so that they're in the valid range
normalizeTree :: CutoffTree -> CutoffTree
normalizeTree (CutoffRule rule) = CutoffRule $ head $ normalizeTunables [rule]
normalizeTree (CutoffBranch cutoff g l) = CutoffBranch cutoff { cutoffValue = normalizedCutoff } normalizedG normalizedL
    where
      normalizedG = normalizeTree g
      normalizedL = normalizeTree l
      normalizedCutoff = scaleToInterval lo_cutoff hi_cutoff current_cutoff
                    
      current_cutoff = abs $ cutoffValue cutoff
      hi_cutoff = cutoffMax $ tunableInfo cutoff
      lo_cutoff = case normalizedL of
                    CutoffRule _                 -> cutoffMin $ tunableInfo cutoff
                    CutoffBranch lowerCutoff _ _ -> abs $ cutoffValue lowerCutoff


instance Show Tunable where
    show (SequentialCutoff (SequentialCutoffInfo name _ min max) val) =
        name ++ " = " ++ show val ++ "\t # valid range: " ++ show min ++ " to " ++ show max
    show (TreeCutoff tree) = show tree

instance Show CutoffTree where
    show (CutoffRule rule) = show rule
    show (CutoffBranch rule g l) = init $ unlines [show rule, show g, show l]


data TunablesConfig = TunablesConfig {  tunables :: [Tunable]  }




-- SERIALIZATION

serializeTunables :: [Tunable] -> String
serializeTunables = unlines . sort . (map show)


data Program = Program { programRoot :: String
                       , programName :: String
                       } deriving Show


-- runs the given PetaBricks progrtam with the --name parameter and returns
-- the results if the run succeeds
programMainName :: Program -> IO (Either String String)
programMainName (Program root name) =
    do (exitCode, stdout, stderr) <- readProcessWithExitCode (root ++ "/" ++ name) ["--name"] ""
       case exitCode of
         ExitSuccess   -> return $ Right $ init stdout
         ExitFailure _ -> return $ Left stderr

resetConfig :: Program -> IO (Either String String)
resetConfig (Program root name) =
    do (exitCode, stdout, stderr) <- readProcessWithExitCode (root ++ "/" ++ name) ["--reset"] ""
       case exitCode of
         ExitSuccess   -> return $ Right $ stdout
         ExitFailure _ -> return $ Left stderr
                                           


data ExecutionStats = ExecutionStats { count :: Int
                                     , average :: Double
                                     , total :: Double
                                     , statsMin :: Double
                                     , statsMax :: Double
                                     , median :: Double
                                     , variance :: Double
                                     , stddev :: Double
                                     } deriving (Show)


timeDefaultExecution :: Int -> Int -> Program -> IO (Either String ExecutionStats)
timeDefaultExecution n trials p@(Program root name) =
    do resetResult <- resetConfig p
       case resetResult of
         Left msg -> return $ Left msg 
         Right _  ->  do (exitCode, stdout, stderr) <- readProcessWithExitCode (root ++ "/" ++ name)
                                                      ["-n", show n, "--time", "--trials", show trials] ""
                         case exitCode of
                           ExitSuccess   -> return $ Right (parseTimingOutput stdout)
                           ExitFailure _ -> return $ Left stderr



timeExecution :: Int -> Int -> Double -> Program -> IO (Either String ExecutionStats)
timeExecution n trials maxSec (Program root name) =
    do (exitCode, stdout, stderr) <- readProcessWithExitCode (root ++ "/" ++ name)
                                     ["-n", show n, "--time", "--trials", show trials, "--max-sec", show maxSec] ""
       case exitCode of
         ExitSuccess   -> return $ Right (parseTimingOutput stdout)
         ExitFailure _ -> return $ Left stderr


parseTimingOutput :: String -> ExecutionStats
parseTimingOutput xmlString = head freshParse
    where
      freshParse = map getTimingInfo $ (tag "root" /> tag "stats" /> tag "timing") doc

      doc = (getContent . xmlParse "timing run") xmlString


getTimingInfo :: Content -> ExecutionStats
getTimingInfo doc = ExecutionStats count average total min max median variance stddev
    where
      count = read $ getattr "count" doc
      average = read $ getattr "average" doc
      -- as of 11/17/2009, total seems to have been removed from petabricks
      -- total = read $ getattr "total" doc
      total = 0
      min = read $ getattr "min" doc
      max = read $ getattr "max" doc
      median = read $ getattr "median" doc
      variance = read $ getattr "variance" doc
      stddev = read $ getattr "stddev" doc



-- writes tunables to program's config file
saveTunables :: [Tunable] -> Program -> IO ()
saveTunables ts p = writeFile path $ serializeTunables ts
    where
      path = (programRoot p) ++ "/" ++ (programName p) ++ ".cfg"


-- times a program using the given values of Tunable parameters
timeConfiguration :: Int -> Int -> Double -> [Tunable] -> Program -> IO (Either String ExecutionStats)
timeConfiguration n trials maxSec tunables p = do saveTunables tunables p
                                                  timeExecution n trials maxSec p
                                          

timeJason :: Int -> Int -> Program -> IO (Either String ExecutionStats)
timeJason n trials (Program root name) =
    do (exitCode, stdout, stderr) <- readProcessWithExitCode (root ++ "/" ++ name)
                                     ["-n", show n, "--time", "--trials", show trials, "--config", root ++ "/" ++ name ++ ".cfg.jason"] ""
       case exitCode of
         ExitSuccess   -> return $ Right (parseTimingOutput stdout)
         ExitFailure _ -> return $ Left stderr
