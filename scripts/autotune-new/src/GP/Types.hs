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
  Types.hs - type operations for the language-description metalanguage
--}

module GP.Types 
    (
      Type(..)
    , isPrimitive
    , isPolymorphic
    , isTypeVariable
    , isTypeCompatible
    ) where


import Data.List (intersperse, intersect)
import GP.AbstractTypeMatching (Matcher, (==>))


data Type = PrimitiveType  { name :: String }
          | PolymorphicType { name :: String, vars :: [Type]}
          | TypeVariable { name :: String }
            deriving (Eq, Ord)


instance Show Type where
    show (PrimitiveType name)              = name
    show (PolymorphicType name vars)       = "(" ++ name ++ " " ++ (concat . intersperse " " . map show) vars ++ ")"
    show (TypeVariable name)               = name


isPrimitive, isPolymorphic, isTypeVariable :: Type -> Bool
isPrimitive (PrimitiveType _)       = True
isPrimitive _                       = False


isPolymorphic (PolymorphicType _ _) = True
isPolymorphic _ = False


isTypeVariable (TypeVariable _)     = True
isTypeVariable _                    = False


-- a `isTypeCompatible` b iff a can be substituted for b
isTypeCompatible :: Type -> Type -> Bool
(PrimitiveType nameA)         `isTypeCompatible` (PrimitiveType nameB)         = nameA == nameB
(TypeVariable _)              `isTypeCompatible` _                             = True
_                             `isTypeCompatible` (TypeVariable _)              = True

(PolymorphicType nameA varsA) `isTypeCompatible` (PolymorphicType nameB varsB) = 
    nameA        == nameB             &&
    length varsA == length varsB      && 
    all (\(a, b) -> a `isTypeCompatible` b) (zip varsA varsB)

_                              `isTypeCompatible` _                            = False
