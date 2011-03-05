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
  Parses grammars
-}


module GP.GrammarParser
    (
      parseGrammar
    ) where


import Text.ParserCombinators.Parsec
import GP.Generator (Term(..), Expansion(..))
import GP.Types (Type(..))
import GP.Possibly (Possibly(..))
import GP.Utils (partitionEithers)

import Debug.Trace


skipWhitespace :: Parser ()
skipWhitespace = skipMany (space <|> tab)


parseTypeVariable :: Parser Type
parseTypeVariable = do tag <- char '$'
                       name <- many letter
                       return $ TypeVariable name


identifier :: Parser String
identifier = many1 (letter <|> char '_' <|> digit)

parsePrimitiveType :: Parser Type
parsePrimitiveType = do name <- identifier
                        return $ PrimitiveType name


parsePolymorphicType :: Parser Type
parsePolymorphicType = do char '['
                          name <- identifier
                          vars <- many (skipWhitespace >> parseType)
                          skipWhitespace
                          char ']'
                          return $ PolymorphicType name vars


parseType :: Parser Type
parseType = do result <- (parsePolymorphicType <|> parseTypeVariable <|> parsePrimitiveType)
               skipWhitespace
               return result
                         


parseTerminal :: Parser Term
parseTerminal = do val <- many1 (noneOf ",: \n\t")
                   skipWhitespace
                   return $ TerminalTerm val


parseGeneratedTerm :: Parser Term
parseGeneratedTerm = do char '['
                        skipWhitespace
                        val <- identifier
                        skipWhitespace
                        char ']'
                        skipWhitespace
                        return $ GeneratedTerm "" val


parseAttribute :: Parser (String, String)
parseAttribute = do char ','
                    skipWhitespace
                    name <- identifier
                    skipWhitespace
                    char '='
                    skipWhitespace
                    val <- identifier
                    skipWhitespace
                    return $ (name, val)


parseNonterminal :: Parser Term
parseNonterminal = do char '<'
                      skipWhitespace
                      t <- parseType

                      attrs <- many parseAttribute
                      
                      -- crossover hook defined?
                      let ch = case lookup "ch" attrs of
                                 Nothing -> ""
                                 Just name -> name

                      char '>'
                      locked <- option False (do
                                               char '*'
                                               return True
                                             )
                      skipWhitespace
                      return $ NonterminalTerm t locked ch


parseTerm :: Parser Term
parseTerm = do parseNonterminal <|> parseGeneratedTerm <|> parseTerminal


parseExpansion :: Parser Expansion
parseExpansion = do skipWhitespace
                    terms <- many parseTerm
                    skipWhitespace
                    string "::"
                    skipWhitespace
                    requiredType <- parseType
                    skipWhitespace
                    return $ Expansion terms requiredType
                                        

parseGrammar :: String -> Possibly [Expansion]
parseGrammar input = case partitionEithers $ map (parse parseExpansion "Expansion") (lines input) of
                       ([], rights) -> Good  $ map (\(Right expansion) -> expansion) rights
                       (lefts, _)   -> Error $ (unlines . map (\(Left err) -> show err ++ "\n")) lefts


main :: IO ()
main = do contents <- readFile "/home/maciej/dev/improve/languages/math.grm"
          case parseGrammar contents of
            Good expansions -> putStrLn $ show expansions
            Error msg       -> putStrLn msg
