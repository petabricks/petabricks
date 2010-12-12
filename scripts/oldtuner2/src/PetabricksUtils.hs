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

module PetabricksUtils
    (
     showCallGraph
    ) where


import Petabricks
import Debug.Trace
import qualified Data.Map as Map


{--
  CALL GRAPH
--}


-- gets transform that are never called
entryPoints :: [TransformInfo] -> [TransformInfo]
entryPoints ts = foldr (\t entries -> sub entries (calleeNames $ transformCallees t)) ts ts
    where
      calleeNames :: [CalleeInfo] -> [String]
      calleeNames = map calleeName

      sub :: [TransformInfo] -> [String] -> [TransformInfo]
      sub transforms callees = filter (not . (`elem` callees) . transformName) transforms

transformNames :: [TransformInfo] -> [String]
transformNames = map transformName

showCallGraph :: String -> [TransformInfo] -> String
showCallGraph mainName transforms = unlines $ map (snd . (print [] 0)) [mainName `transformByName` transforms]
    where
      makeIndent n = "\n" ++ (replicate n ' ') ++ "- "
                   
      -- recursively serializes a TransformInfo. Returns a pair of visited nodes in the call graph
      -- and their string representation
      print :: [TransformInfo] -> Int -> TransformInfo -> ([TransformInfo], String)
      print visited level t@(TransformInfo name tunables callees)  =
          if t `elem` visited
          then 
               (visited, indent ++ name ++ " (...)")
          else
              (vs, str)
              where
                indent = makeIndent (2*level)
                
                safeInit xs = if null xs then [] else init xs

                str = indent ++ name ++  " (tunables: " ++ (show . length) tunables
                      ++ ", callees " ++ (show . length) callees ++ ")"
                      ++ concat children

                (vs, children) = foldr (\(CalleeInfo name) (vs, ss) ->
                                            let (vs', s) = print vs (level + 1) (name `transformByName` transforms)
                                            in
                                              (vs', [s] ++ ss)
                                       )
                                 ([t] ++ visited, [])
                                 callees


