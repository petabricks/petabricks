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
  Various utility functions that are used commonly yet are not part of
  a discernable bigger abstraction.
--}

module GP.Utils
    (
     choose
    , xor
    , partitionEithers
    , indexFoldr
    , fact
    , scaleToInterval
    ) where


import Data.List (partition)

-- given a list of indices, takes the first one and returns an element of a list at that
-- index. The index is wrapped around to make sure it doesn't go out of bounds.
-- 
-- WARNING: the list has to be finite
choose :: [Int] -> [a] -> a
choose choices xs
    | size == 0        = error "No items to choose from (list is empty)"
    | otherwise        = xs !! ((`mod` size) . head) choices
    where
      size = length xs


-- binary exclusive or
xor :: Bool -> Bool -> Bool
xor x y = x /= y


-- returns all Lefts
lefts :: [Either a b] -> [Either a b]
lefts = filter (either (\_ -> True) (\_ -> False))


-- returns all Rights
rights :: [Either a b] -> [Either a b]
rights = filter (either (\_ -> False) (\_ -> True))


-- splits eithers into a pair of Lefts and Rights, respectively
partitionEithers :: [Either a b] -> ([Either a b], [Either a b])
partitionEithers = partition (either (\_ -> True) (\_ -> False))


-- like foldr, but passes the index of iteration to the folding function, where the first element
-- in the list has index 0.
indexFoldr :: (Int -> a -> b -> b) -> b -> [a] -> b
indexFoldr f initial xs = snd $ foldr folder (length xs - 1, initial) xs
                          where
                            folder = (\x (index, sofar) -> (index - 1, f index x sofar))



-- computes the factorial of a number
fact :: Int -> Int
fact n
     | n <= 0    = 1
     | otherwise = n * fact (n - 1) 




scaleToInterval :: Int -> Int -> Int -> Int
scaleToInterval min max val = round $ (f min) + (f val') * ((f max) - (f min)) /  (f (maxBound :: Int))
    where
      f = fromIntegral
      -- abs will be negative if val = min_int, since min_int = -2^N, whereas max_int = 2^N-1, so
      -- multiplying min_int by -1 will overflow. This makes sure abs never overflows
      val' = if val == (minBound :: Int) then abs(val + 1) else abs val