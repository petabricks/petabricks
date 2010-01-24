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

 {--
  Syntax tree datatypes and generation algorithms.
-}


module GP.Generator
    (
      isNonterminal
    , isTerminal
    , Term (..)
    , Expansion (..)
    , SyntaxTree (..)
    , GeneratorState (stateChoices)
    , UserGenerator (..)
    , hasNonterminals
    , expandDependentTerms
    , expandFreeTerms
    , expand
    , startState
    , flattenTree
    ) where


import Debug.Trace (trace)
import System.Random (mkStdGen, randoms, getStdGen)
import Data.List (intersperse, find)
import Data.Map (Map, fromList, (!), member, insert)
import GP.Types (Type (..), isPrimitive, isPolymorphic, isTypeVariable, isTypeCompatible)
import GP.Possibly (Possibly (Good, Error))
import GP.Utils (choose)



{-
  TERMINALS & NONTERMINALS
-}



data Term = NonterminalTerm { termRequiredType :: Type, locked :: Bool, crossoverHookName :: String } 
          | TerminalTerm    { termName :: String }
          | GeneratedTerm   { termName :: String, generatorName :: String }
            deriving (Eq)


instance Show Term where
    show (NonterminalTerm requiredType locked ch) = show requiredType ++ if locked then "*" else ""
                                                    ++ if (not . null) ch
                                                       then ", crossover hook: " ++ ch
                                                       else ""
    show (TerminalTerm name)            = "\"" ++ name ++ "\""
    show (GeneratedTerm name gen)       = "[\"" ++ name ++ "\" <- " ++ gen ++ "]"


isNonterminal,isTerminal :: Term -> Bool
isNonterminal (NonterminalTerm _ _ _) = True
isNonterminal _                       = False

isTerminal = not . isNonterminal



{-
  EXPANSIONS
-}



-- represents one possible expansion of a nonterminal
data Expansion = Expansion
    {
      expansionTerms :: [Term]
    , expansionType :: Type
    }


hasNonterminals :: Expansion -> Bool
hasNonterminals = any isNonterminal . expansionTerms


instance Show Expansion where
    show (Expansion terms expansionType) = "{ " ++ (concat . intersperse " " . map show) terms ++ " }::"
                                           ++ show expansionType



{-
  SYNTAX TREES
-}



-- represents syntax trees generated from the meta grammar
data SyntaxTree = Branch { branchTerm :: Term,                  -- the term that the branch was expanded from
                           branchType :: Type,                  -- type of the branch
                           branchChildren :: [SyntaxTree] }     -- subtrees
                | Leaf   { leafValue :: String }
                  deriving (Eq)


instance Show SyntaxTree where
    show = ("\n" ++) . printSyntaxTree 1 10
--    show tree = "SyntaxTree"


-- shows terminals from all leaves separated by spaces
flattenTree :: SyntaxTree -> String
flattenTree (Leaf value) = value
flattenTree (Branch _ _ children) = let str = (concat . intersperse " " . map flattenTree) children
                                    in
                                      if length children == 1
                                      then str
                                      else "(" ++ str ++ ")"


-- pretty prints a syntax tree
printSyntaxTree :: Int -> Int -> SyntaxTree -> String
printSyntaxTree ident depth tree
    | depth == 0     = "- (...)"
    | otherwise      = let header = printSyntaxTreeHeader tree in
                       case tree of                                
                         (Leaf value)             -> "- " ++ header
                         (Branch term t children) ->
                             "- " ++ header ++ "\n" ++
                                 (concat . map (identString ++)) subtrees
                             where
                               identString = replicate ident '\t'
                               subtrees = (intersperse "\n" . map (printSyntaxTree (ident + 1) (depth - 1))) children
                                

-- prints the header of a syntax tree, without any leading or trailing whitespace characters
printSyntaxTreeHeader :: SyntaxTree -> String
printSyntaxTreeHeader tree = case tree of
                               (Leaf value)         -> "\"" ++ value ++ "\""
                               (Branch term t _)    -> show t ++ ", from " ++ show (termRequiredType term)



{-
  SYNTAX TREE GENERATION
-}



-- creates an error string indicating type mismatch
showTypeMismatch :: Type -> Type -> String
showTypeMismatch expected actual = "Type mismatch. Expected: " ++ show expected ++ ". Actual: " ++ show actual


-- allows the user to supply custom code to generate non-terminals
data UserGenerator a = UserGenerator
    {
      -- generates a new term given the custom state and current name of the terminal
      generate :: a -> String -> (a, String)
    }


-- holds state necessary for syntax tree generation
data GeneratorState a = GeneratorState
    {
      stateExpansions   :: [Expansion]      -- all expansions available to the generator
    , stateVariableMap  :: Map Type Type    -- maps instantiated type variables to concrete types
    , stateChoices      :: [Int]            -- an infinite list denoting which expansion to choose at each step
    , stateCount        :: Int              -- number of times state has been changed, used for debugging
    , stateDepth        :: Int              -- current depth in the tree
    , stateMaxDepth     :: Int              -- maximum tree depth
    , userGenerators    :: Map String (UserGenerator a, a)
    }



{-
  STATE TRANSFORMATIONS
-}



-- creates an initial state that can be used to generate syntax trees
startState :: [Expansion] -> [Int] -> Int -> [(String, UserGenerator a, a)] -> GeneratorState a
startState expansions choices maxDepth userGenerators = GeneratorState {
                                           stateExpansions = expansions
                                         , stateVariableMap = fromList []
                                         , stateChoices = choices
                                         , stateCount = 0
                                         , stateDepth = 0
                                         , stateMaxDepth = maxDepth
                                         , userGenerators = fromList $ map (\(a, b, c) -> (a, (b, c))) userGenerators
                                         }

-- removes the head choice from a GeneratorState
advanceChoices :: GeneratorState a -> GeneratorState a
advanceChoices state = state { stateChoices = tail $ stateChoices state, stateCount = (+1) $ stateCount state }


-- clears type variable map in a generator state
clearInstances :: GeneratorState a -> GeneratorState a
clearInstances state = state { stateVariableMap = fromList [] }


-- increases the depth in a generator state
deepen :: GeneratorState a -> GeneratorState a
deepen state = state { stateDepth = 1 + stateDepth state }


-- copies choices from one state to another
copyChoices :: GeneratorState a -> GeneratorState a -> GeneratorState a
copyChoices src dest = dest { stateChoices = stateChoices src, stateCount = stateCount src, userGenerators = userGenerators src }


-- given a state at one level, creates state that can be used for generating children one
-- level below
childrenState :: GeneratorState a -> GeneratorState a
childrenState = deepen . clearInstances


-- given an original type and a new one, creates type variable bindings in the generator state
-- that would transform the first into the latter 
instantiateState :: Type -> Type -> GeneratorState a -> GeneratorState a
instantiateState originalType newType originalState =
    case originalType of
      (PrimitiveType _)      -> originalState
      v@(TypeVariable  _)    -> case newType of
                                  (TypeVariable _) -> originalState
                                  _                -> originalState {
                                                        stateVariableMap = insert v newType $ stateVariableMap originalState
                                                      }
      (PolymorphicType _ _)  -> foldr instantiator originalState $ zip (vars originalType) (vars newType)
                               where
                                 -- Creates type variable bindings in generator state
                                 -- before: type before instantiation
                                 -- after:  type after instantiation
                                 instantiator :: (Type, Type) -> GeneratorState a -> GeneratorState a
                                 instantiator = (\(before, after) state ->
                                                     let varMap = (stateVariableMap state)
                                                     in
                                                       -- is the variable already bound?
                                                       if member before varMap && (varMap ! before) /= after
                                                       then error $ showTypeMismatch (varMap ! before) after
                                                       else instantiateState before after state
                                                )

-- subtitutes type variables in a type using variable map from a generator state
instantiateType :: GeneratorState a -> Type -> Type
instantiateType state t = case t of
                            t@(PrimitiveType _)         -> t
                            t@(TypeVariable _ )         -> if member t (stateVariableMap state)
                                                           then (stateVariableMap state) ! t
                                                           else t
                            (PolymorphicType name vars) -> (PolymorphicType name (map (instantiateType state) vars))


-- gets all expansions that can be substituted for the given type
compatibleExpansions :: Type -> [Expansion] -> [Expansion]
compatibleExpansions t = filter $ (`isTypeCompatible` t) . expansionType


-- chooses a type-compatible expansion for the given type, if any
chooseExpansion :: GeneratorState a -> Type -> Possibly (Expansion, GeneratorState a)
chooseExpansion state t = if null exs
                          then Error $ "Could not find a compatible expansion for type " ++ show t ++
                                   " at depth " ++ show (stateDepth state)
                          else Good $ (instantiateExpansion state t (choose (stateChoices state) exs), advanceChoices state)
                              where allCompatible = compatibleExpansions t (stateExpansions state)
                                    exs = if (stateDepth state) == (stateMaxDepth state) - 1
                                          then filter (not . hasNonterminals) allCompatible
                                          else allCompatible


-- expands a list of terms, modifying the state accordingly
expandDependentTerms :: GeneratorState a -> [Term] -> Possibly ([SyntaxTree], GeneratorState a)
expandDependentTerms initState []     = Good ([], initState)
expandDependentTerms initState (t:ts) = do (tree, nextState)   <- expand initState t
                                           (trees, finalState) <- expandDependentTerms nextState ts
                                           return (tree:trees, finalState)


-- expands terms that are independent of each other, i.e. variable instantiations due to expansion
-- of one term should not affect the other etc. The initial state for each expansion doesn't change
-- except for the list of choices, which is advanced for all expansions.
expandFreeTerms :: GeneratorState a -> [Term] -> Possibly ([SyntaxTree], GeneratorState a)
expandFreeTerms initState []     = Good ([], initState)
expandFreeTerms initState (t:ts) = do (tree, nextState)   <- expand initState t
                                      (trees, finalState) <- expandFreeTerms (copyChoices nextState initState) ts
                                      return (tree:trees, copyChoices finalState initState)


-- given a required type for an expansion, substitutes concrete values for type variables in that expansion
-- If both expansions share type variable names, they will be renamed before substitution
instantiateExpansion :: GeneratorState a -> Type -> Expansion -> Expansion
instantiateExpansion state requiredType ex = Expansion terms $ (instantiateType newState . expansionType) ex
                                             where
                                               newState = instantiateState (expansionType ex) requiredType state
                                               terms = map termMapper (expansionTerms ex)
                                                       
                                               termMapper t@(TerminalTerm _) = t
                                               termMapper t@(GeneratedTerm _ _) = t
                                               termMapper (NonterminalTerm originalType locked chook) =
                                                   NonterminalTerm (instantiateType newState (makeVariablesDistinct requiredType originalType)) locked chook


-- given 2 types, renames variable types in the second one so that the two have none in common
makeVariablesDistinct :: Type -> Type -> Type
makeVariablesDistinct other this = case this of
                                     (TypeVariable name) -> (TypeVariable $ uniqueName name 2 otherNames)
                                     (PolymorphicType name vars) ->
                                         (PolymorphicType name (map (makeVariablesDistinct other) vars))
                                     t -> t
                                   where
                                     otherNames = variableNames other
                                     

-- gets names of all type variables in a type. The list can contain duplicates
variableNames :: Type -> [String]
variableNames (PrimitiveType _) = []
variableNames (TypeVariable name) = [name]
variableNames (PolymorphicType _ vars) = (concat . map variableNames) vars


-- given a prototype name and a suffix, increments the suffix as many times as necessary to make
-- (prototype ++ show suffix) name distinct from any of the names in a given list
uniqueName :: String -> Int -> [String] -> String
uniqueName prototype suffix others = if try `notElem` others
                                     then try
                                     else uniqueName prototype (suffix + 1) others
                                         where
                                           try = prototype ++ show suffix


-- generates a syntax tree which is type-compatible with the given term
expand :: GeneratorState a -> Term -> Possibly (SyntaxTree, GeneratorState a)
expand state (TerminalTerm name) = Good (Leaf name, state)

expand state t@(GeneratedTerm val genName) = if genName `member` (userGenerators state)
                                             then Good (Leaf generatedVal, updatedGeneratorState)
                                             else error $ "User generator not available: " ++ genName
    where
      (gen, startState) = (userGenerators state) ! genName
      (endState, generatedVal)   = (generate gen) startState val
      updatedGeneratorState = state {
                                userGenerators = insert genName (gen, endState) (userGenerators state)
                              }

expand initState term@(NonterminalTerm requiredType locked _)
           | stateDepth initState >= stateMaxDepth initState = Error "Maximum depth exceeded"
           | otherwise =
               do
                 (expansion, state)             <- chooseExpansion initState (instantiateType initState requiredType)
                 (children, childrenFinalState) <- expandDependentTerms (childrenState state) (expansionTerms expansion)
                                                   
                 let finalExpansionType = instantiateType childrenFinalState (expansionType expansion)
                                          
                 Good (Branch term finalExpansionType children,
                       instantiateState requiredType finalExpansionType (copyChoices childrenFinalState initState))



