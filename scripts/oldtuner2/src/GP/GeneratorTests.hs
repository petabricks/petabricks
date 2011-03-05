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

import Test.QuickCheck
import Control.Monad
import Generator
import Types
import Utils (xor)

{-
  TYPES
-}

capitalString :: Gen String
capitalString = do
  firstChar <- elements ['A' .. 'Z']
  rest <- arbitrary
  return $ firstChar : rest

arbitraryList :: (Arbitrary a) => (Int, Int) -> Gen [a]
arbitraryList range = do
  n <- choose range :: Gen Int
  mapM (\x -> arbitrary) (replicate n 0)

instance Arbitrary Type where
    coarbitrary = undefined
    arbitrary = oneof
                [ liftM PrimitiveType capitalString
                , do
                    varName <- elements ['a' .. 'z']
                    return $ TypeVariable (varName : [])
                , do
                    vars <- arbitraryList (1,2)
                    name <- capitalString
                    return $ PolymorphicType name vars
                ]

{-
  TERMINALS & NONTERMINALS
-}

instance Arbitrary Char where
    coarbitrary = undefined
    arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'])


instance Arbitrary Term where
    coarbitrary = undefined
    arbitrary =
        oneof
        [ liftM TerminalTerm arbitrary
        , liftM NonterminalTerm arbitrary
        ]


instance Arbitrary Expansion where
    coarbitrary = undefined
    arbitrary = do
      terms <- arbitraryList (0,10)
      t <- arbitrary
      return $ Expansion terms t


prop_either_terminal x = (isTerminal x) `xor` (isNonterminal x)

prop_empty_has_nonterminals x = null (expansionTerms x) ==> not $ hasNonterminals x
