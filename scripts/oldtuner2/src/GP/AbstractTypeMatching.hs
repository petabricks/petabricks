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
 Provides simple matching on types without directly inspecting contents 
--}

module GP.AbstractTypeMatching
    (
     Matcher
   , (==>)
    ) where

type Matcher a b = a -> Maybe b

-- Matcher combinator. Given two matchers, creates a new matcher which calls
-- the first one and then the second one only if the result of the first is Nothing
(==>) :: Matcher a b -> Matcher a b -> Matcher a b
firstMatcher ==> secondMatcher = chainedMatcher where
    chainedMatcher arg =
        case fstResult of
          Nothing -> secondMatcher arg
          x@(Just _) -> fstResult
        where fstResult = firstMatcher arg
