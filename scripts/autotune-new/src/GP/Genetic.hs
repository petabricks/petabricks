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

{-
  Population generation and evolution
-}


module GP.Genetic
    (
      startEvolving
    , Evolver (..)
    , Evaluator (..)
    , EvolutionReporter
    , EvaluatedSyntaxTree (..)
    , GenerationMerger
    , StopCondition
    , CrossoverHook (..)
    , bestMember
    , averageFitness
    ) where


import GP.Generator
import GP.Possibly (Possibly (..), possibly)
import GP.Types (Type (..), isTypeCompatible)
import GP.GrammarParser (parseGrammar)
import GP.Utils (indexFoldr)
import System.Random (mkStdGen, randoms, randomRs)
import Control.Monad.State
import Data.Map (Map, fromList, (!), member, insert)
import Debug.Trace (trace)


type StopCondition = [EvaluatedSyntaxTree] -> Bool




-- allows precise control over crossover
data CrossoverHook a = CrossoverHook { crossoverHook         :: a -> SyntaxTree -> SyntaxTree -> (a, SyntaxTree)
                                       
                                     }

hasHook :: Evolver a b c -> String -> Bool
hasHook state name = name `member` (crossoverHooks state)

callCrossoverHook :: String -> SyntaxTree -> SyntaxTree -> EvolverState a b c SyntaxTree
callCrossoverHook hookName tree1 tree2 = do state <- get
                                            let (hook, hookState) = (crossoverHooks state) ! hookName
                                                (newHookState, offspring) = (crossoverHook hook) hookState tree1 tree2
                                            put state {
                                                      crossoverHooks = insert hookName
                                                                       (hook, newHookState)
                                                                       (crossoverHooks state)
                                                    }
                                            return offspring



data Evolver a b c = Evolver { choices                       :: [Int]
                             , probabilities                 :: [Double]
                             , grammar                       :: [Expansion]
                             , maxTreeDepth                  :: Int
                             , mutationProbability           :: Double
                             , completeMutationProbability   :: Double
                             , randomSelectionProbability    :: Double
                             , evaluator                     :: Evaluator a
                             , populationSize                :: Int
                             , merger                        :: GenerationMerger a
                             , stopCondition                 :: StopCondition
                             , generationNumber              :: Int
                             , userState                     :: a
                             , userGenerators                :: [(String, UserGenerator b, b)]
                             , crossoverHooks                :: Map String (CrossoverHook c, c)
                             }



type EvolverState a b c d  = State (Evolver a b c) d


-- transforms an evolver state into a generator state. No evolver state is modified
mkGeneratorState :: EvolverState a b c (GeneratorState b)
mkGeneratorState = do state <- get
                      return $ startState (grammar state) (choices state) (maxTreeDepth state) (userGenerators state)


-- merges an evolver state with a generator state by copying the generator's random numbers
-- to the evolver's state 
mergeStates :: GeneratorState b -> EvolverState a b c ()
mergeStates genState = do evoState <- get
                          put evoState { choices = stateChoices genState }


-- gets a random integer from the evolver's internal state
randInt :: EvolverState a b c Int
randInt = do state <- get
             let r = head $ choices state
             put state { choices = tail $ choices state }
             return r

-- gets a random double 0-1 from the evolver's internal state
randDouble :: EvolverState a b c Double
randDouble = do state <- get
                let r = head $ probabilities state
                put state { probabilities = tail $ probabilities state }
                return r

-- chooses a random element from a list
randElt :: [a] -> EvolverState b c d a
randElt lst = do n <- randInt
                 let index = n `mod` (length lst)
                 return $ lst !! index


-- increments population number
incGenerationNumber :: EvolverState a b c ()
incGenerationNumber = do state <- get
                         put state { generationNumber = (generationNumber state) + 1  }
                         return ()


-- Expands the given term N times, generating N syntax trees
generatePopulation :: Term -> EvolverState a b c (Possibly [SyntaxTree])
generatePopulation startTerm = do genState <- mkGeneratorState
                                  evoState <- get
                                  let n = populationSize evoState
                                      genResult = expandFreeTerms genState $ replicate n startTerm
                                  case genResult of
                                       Error msg -> return $ Error ("Could not generate population:\n\t" ++ msg)
                                       Good (trees, genState') -> do mergeStates genState'
                                                                     return $ Good trees
                                                                         



data Subtree = Subtree { subtree :: SyntaxTree
                       , path    :: [Int]       -- path from root, each number represents a child index
                       }

instance Show Subtree where
    show sub = "\nPath: " ++ (show . path) sub ++ "\nSubtree:" ++ (show . subtree) sub


-- advances path to the subtree by one node
advancePath :: Subtree -> Subtree
advancePath subtree = subtree { path = (tail . path) subtree }


-- Gets all subtrees of the given syntax tree, without leaves. The first element is guaranteed to be the root
-- tree
subtrees :: SyntaxTree -> [Subtree]
subtrees = subtrees' []
    where
      subtrees' :: [Int] -> SyntaxTree -> [Subtree]
      subtrees' _    (Leaf _) = []
      subtrees' path t@(Branch _ _ children) = (Subtree t path) : recurse children
          where
            recurse :: [SyntaxTree] -> [Subtree]
            recurse xs = indexFoldr
                         (\index elt subs ->
                              (subtrees' (path ++ [index]) elt) ++ subs)
                         []
                         xs


-- gets all subtrees compatible with the given type
compatibleSubtrees :: Type -> [Subtree] -> [Subtree]
compatibleSubtrees requiredType subs = filter (isCompatible . branchType . subtree) subs
                                       where
                                         isCompatible = (`isTypeCompatible` requiredType)


-- applies f only to the element at the given index, leaving the rest untouched
applyAtIndex :: (a -> a) -> Int -> [a] -> [a]
applyAtIndex f i = indexFoldr (\index elt sofar -> if index == i then (f elt) : sofar else elt : sofar) []


-- replaces a subtree in a syntax tree with a given syntax tree
replace :: SyntaxTree -> Subtree -> SyntaxTree -> SyntaxTree
replace root toReplace replaceWith =
    case root of
      t@(Leaf _)              -> error "Cannot replace a leaf"
      t@(Branch _ _ children) -> if null (path toReplace)
                                 then replaceWith
                                 else t { branchChildren = applyAtIndex
                                                           recurse
                                                           (head (path toReplace)) children
                                        }
        where
          recurse child
              | null (path toReplace) = replaceWith
              | otherwise             = replace child (advancePath toReplace) replaceWith



{-
  GENETIC OPERATORS - MUTATE & CROSSOVER
-}

-- returns the first list if not null, otherwise returns the second one
safeNull :: [a] -> [a] -> [a]
safeNull xs ys
    | null xs   = ys
    | otherwise = xs


mergeHistories :: [Double] -> [Double] -> [Double]
mergeHistories [] y = y
mergeHistories x [] = x
mergeHistories (x:xs) (y:ys) = (x + y)/2:mergeHistories xs ys

-- performs a genetic crossover of 2 syntax trees. The returned tree is the first tree
-- with a random subtree replaced by a compatible one from the second tree
crossover :: EvaluatedSyntaxTree -> EvaluatedSyntaxTree -> EvolverState a b c EvaluatedSyntaxTree
crossover tree1 tree2 = do let eligibles = filter (not . locked . branchTerm . subtree) (subtrees $ tree tree1)
                           sub_tree1 <- randElt eligibles
                                        
                           let hookName = crossoverHookName $ branchTerm $ subtree sub_tree1
                               requiredType = ((branchType . subtree) sub_tree1)
                               compatibles = safeNull (compatibleSubtrees requiredType (subtrees $ tree tree2))
                                                      (compatibleSubtrees requiredType (subtrees $ tree tree1))

                           sub_tree2 <- randElt compatibles
                           state <- get

                           if null hookName
                               then
                                   return EvaluatedSyntaxTree {
                                                tree = replace (tree tree1) sub_tree1 (subtree sub_tree2)
                                              , fitness = -1
                                              , fitnessHistory = mergeHistories (fitnessHistory tree1) (fitnessHistory tree2)
                                              }
                               else if hasHook state hookName
                                    then
                                        do offspring <- callCrossoverHook hookName (subtree sub_tree1) (subtree sub_tree2)
                                           return EvaluatedSyntaxTree {
                                                        tree = replace (tree tree1) sub_tree1 offspring
                                                      , fitness = -1
                                                      , fitnessHistory = mergeHistories (fitnessHistory tree1) (fitnessHistory tree2)
                                                      }
                                    else
                                        error $ "Crossover hook not available: " ++ hookName
    

-- mutates a random subtree with the probability given in the evolver state
mutate :: EvaluatedSyntaxTree -> EvolverState a b c EvaluatedSyntaxTree
mutate atree = do pr <- randDouble
                  state <- get
                  if pr > mutationProbability state
                     then
                         return atree
                     else
                         do r1 <- randDouble
                            
                            toMutate <- if r1 < completeMutationProbability state
                                        then return $ (subtrees (tree atree)) !! 0
                                        else randElt $ subtrees (tree atree)

                            let regenerated = (subtree toMutate) == (tree atree)

                            genState <- mkGeneratorState
                            case subtree toMutate of
                              Leaf _ -> return atree
                              Branch term reqdType _ ->
                                  case expand genState term { termRequiredType = reqdType } of
                                    Error msg                 -> return $ trace ("Could not mutate: " ++ msg) atree
                                    Good (genTree, genState') -> do mergeStates genState'
                                                                    return atree { tree = replace (tree atree) toMutate genTree
                                                                                 , fitnessHistory = if regenerated
                                                                                                    then []
                                                                                                    else fitnessHistory atree
                                                                                 }

{-
  FITNESS EVALUATION
-}

-- Evaluates a SyntaxTree and returns a fitness value. Since SyntaxTrees are programs,
-- evaluation can have arbitrary side effects and is therefore wrapped in the IO monad
newtype Evaluator a = Evaluator {
         runEval :: a -> EvaluatedSyntaxTree -> IO (a, Double)
      }


data EvaluatedSyntaxTree = EvaluatedSyntaxTree { fitness :: Double
                                               , fitnessHistory :: [Double] -- head -> tail: most recent -> least recent
                                               , tree    :: SyntaxTree
                                               } deriving (Show)

instance Eq EvaluatedSyntaxTree where
    a == b = (tree a) == (tree b)

instance Ord EvaluatedSyntaxTree where
    a `compare` b = (fitness a) `compare` (fitness b)


-- Evaluates fitness of a single tree
evalTree :: EvaluatedSyntaxTree -> Evolver a b c -> IO (EvaluatedSyntaxTree, Evolver a b c)
evalTree atree state = do (ustate, fitnessVal) <- ((runEval . evaluator) state) (userState state) atree
                          return (atree { fitness = fitnessVal
                                        , fitnessHistory = fitnessVal : fitnessHistory atree
                                        }
                                 , state { userState = ustate }
                                 )



-- Evaluates fitness of a population
evaluate :: [EvaluatedSyntaxTree] -> Evolver a b c -> IO ([EvaluatedSyntaxTree], Evolver a b c)
evaluate [] s = return ([], s)
evaluate (p:ps) s = do (ep, s')   <- evalTree p  s
                       (eps, s'') <- evaluate ps s'
                       return (ep:eps, s'')


-- Normalizes fitness values of evaluated syntax trees so that they add up to 1
normalizeFitnesses :: [EvaluatedSyntaxTree] -> [EvaluatedSyntaxTree]
normalizeFitnesses xs = if totalFitness == 0
                        then map (\x -> x { fitness = 0 }) xs
                        else map (\x -> x { fitness = fitness x / totalFitness }) xs
    where
      totalFitness = foldr ((+) . fitness) 0 xs


-- Normalizes fitness values of evaluated syntax trees and picks an element t from them
-- with probability (fitness t). All fitnesses have to be normalized.
pickForReproduction :: (d -> Double) -> [d] -> EvolverState a b c d
pickForReproduction func trees
    = do pr <- randDouble
         let result = fst $ foldr picker ([], pr) trees
         if null result
         -- due to rounding errors, fitnesses might not add up to exactly 1
         -- In such cases, if pr is close to 1, no tree will be picked by the foldr
         -- so just assume the first one should have been picked (remember foldr
         -- goes right -> left).
         -- This happens *very* rarely, so this fix does no harm.
            then return $ head trees
            else return $ head result
    where
      picker t x@(picked, r)
          | null picked = if func t >= r
                          then ([t], r)
                          else ([], r - func t)
          | otherwise   = x



-- Gets population member with the highest fitness value
bestMember :: [EvaluatedSyntaxTree] -> EvaluatedSyntaxTree
bestMember trees = foldr (\x currentMax ->
                              if fitness x > fitness currentMax
                              then x
                              else currentMax)
                   (head trees)
                   trees


-- Gets the average fitness of a population
averageFitness :: [EvaluatedSyntaxTree] -> Double
averageFitness trees
    | null trees = 0
    | otherwise = (foldr ((+) . fitness) 0 trees) / (fromIntegral $ length trees)


-- Function of this type gets syntax trees at each epoch right after evaluation
type EvolutionReporter a = Int -> a -> [EvaluatedSyntaxTree] -> IO (a)


-- Evolves a new syntax tree from a population
evolveTree :: [EvaluatedSyntaxTree] -> EvolverState a b c EvaluatedSyntaxTree
evolveTree trees = do state <- get
                      r1 <- randDouble
                      r2 <- randDouble
                      parent1          <- if r1 >= (randomSelectionProbability state)
                                          then pickForReproduction fitness trees
                                          else randElt trees
                      parent2          <- if r2 >= (randomSelectionProbability state)
                                          then pickForReproduction fitness trees
                                          else randElt trees
                      offspring        <- crossover parent1 parent2
                      mutatedOffspring <- mutate offspring                               
                      return mutatedOffspring


-- evolves a population of syntax trees
evolvePopulation :: Int -> [EvaluatedSyntaxTree] -> EvolverState a b c [EvaluatedSyntaxTree]
evolvePopulation member population
    | null population = return []
    | member == 0     = return []
    | otherwise       = do evolvedTree <- evolveTree population
                           otherEvolvedTrees <- evolvePopulation (member - 1) population
                           return $ evolvedTree : otherEvolvedTrees

       
-- merges an old generation with the new one. Can be used to preserve best members from the old
-- generation etc. The type parameter "a" represents user state
type GenerationMerger a = a -> [EvaluatedSyntaxTree] -> [EvaluatedSyntaxTree] -> ([EvaluatedSyntaxTree], a)

callGenerationMerger :: [EvaluatedSyntaxTree] -> [EvaluatedSyntaxTree] -> EvolverState a b c [EvaluatedSyntaxTree]
callGenerationMerger old new = do s <- get
                                  let ustate = userState s
                                      m = merger s
                                      (merged, ustate') = m ustate old new
                                  put s { userState = ustate' }
                                  return merged


-- evolves a population for a number of epochs.
-- Note: we want this to live inside the IO monad, so we can't use State monad's conveniences
-- (or maybe we can but my haskell vodoo isn't strong enough)
evolve :: Evolver a b c -> Int -> EvolutionReporter a -> [EvaluatedSyntaxTree]
       -> IO (  (Evolver a b c, [EvaluatedSyntaxTree])  )
evolve initState epochs reporter population
    | epochs == 0    = return (initState, population)
    | (stopCondition initState) population = do finalUserState <- reporter (generationNumber initState)
                                                                  (userState initState)
                                                                  population
                                                return (initState { userState = finalUserState }, population)
                                                       
    | otherwise      = do let (evolvedPopulation, evoState) = runState evolveHelper initState
                          (reevaluatedOldPopulation, evoState') <- evaluate population evoState
                          (evaluatedEvolvedPopulation, evoState'') <- evaluate evolvedPopulation evoState'
                          newUserState <- reporter (generationNumber evoState'') (userState evoState'') evaluatedEvolvedPopulation
                          let evoState''' = execState incGenerationNumber evoState'' { userState = newUserState }
                              genMerger = callGenerationMerger reevaluatedOldPopulation evaluatedEvolvedPopulation
                              (nextPopulation, finalEvoState)  = runState genMerger evoState'''
                          evolve finalEvoState (epochs - 1) reporter nextPopulation
    where
      test = []
      evolveHelper :: EvolverState a b c [EvaluatedSyntaxTree]
      evolveHelper = do let normalized = normalizeFitnesses population
                        state <- get
                        evolvedPopulation <- evolvePopulation (populationSize state) normalized
                        return evolvedPopulation
                            


-- generates a new population and evolves it for a number of epochs
startEvolving :: Evolver a b c -> Term -> Int -> EvolutionReporter a -> IO ([EvaluatedSyntaxTree])
startEvolving initState startTerm epochs reporter = do (evaluated, nextState2) <- evaluate initialEvaluatedPopulation nextState
                                                       newUserState <- reporter (generationNumber nextState2)
                                                                       (userState nextState2)
                                                                       evaluated
                                                       (_, finalPopulation) <- evolve nextState2 { userState = newUserState, generationNumber = 1 + generationNumber nextState2 } epochs reporter evaluated
                                                       return finalPopulation
                                                    where
                                                      (initialPopulation, nextState) =
                                                          case runState (generatePopulation startTerm) initState of
                                                              (Error msg, _) ->
                                                                  error $ "Could not generate initial population: " ++ msg
                                                              (Good x, s)    -> (x, s)
                                                      initialEvaluatedPopulation = map (\t -> EvaluatedSyntaxTree {  fitness = -1, fitnessHistory = [], tree = t }) initialPopulation
