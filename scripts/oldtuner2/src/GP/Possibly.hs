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
  Simple monad for error handling
--}

module GP.Possibly
    (
      Possibly(Good, Error)
    , possibly
    ) where

data Possibly a = Good a
                | Error String
                  deriving (Show)

possibly :: (a -> b) -> (String -> b) -> Possibly a -> b
possibly good _     (Good x)    = good x
possibly _    error (Error msg) = error msg

instance Monad Possibly where
    (Good val)  >>= f   = f val
    (Error val) >>= f   = Error val
    return val          = Good val
