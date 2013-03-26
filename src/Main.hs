{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Damien PUIG
-- License     :  AllRightsReserved
--
-- Maintainer  :  Damien
-- Stability   :  0.0.1
-- Portability :  -

-----------------------------------------------------------------------------
-- helped by http://learnyouahaskell.com/chapters
-- helped by "Programming in Haskell", Graham Huton
-- Using Parsec Library http://hackage.haskell.org/packages/archive/parsec/3.0.0/doc/html/Text-Parsec.html
-- Using Parsec Combinator http://hackage.haskell.org/packages/archive/parsec/3.0.0/doc/html/Text-Parsec-Combinator.html
module Main (
    main
) where

import Control.Monad (unless)
import Data.List (stripPrefix)
import Data.Map hiding (map)
import Text.ParserCombinators.Parsec

-- checking spaces on matching
currentparser :: Parser current -> Parser current
currentparser pars = do res <- pars
                        spaces
                        return res

null :: [a] -> Bool
null [] = True
null _ = False

-- Entry point of the program.
-- Get the path of the json file, then parse it.
parseMain = do
          putStrLn  "Enter the path of the json file :"
          entry <- getLine
          if Main.null entry
                    then do putStrLn "Enter path please!"
                            parseMain
                    else do
                    x <- readFile entry
                    putStrLn x
                    save $ parseJSON x -- parsing called

save :: String -> IO ()
save result = do putStrLn result
                 putStrLn "the result is going to be saved. Can you please give a name to the new file?"
                 filename <- getLine
                 writeXml (filename, result)
                 putStrLn "Exiting..."

writeXml ::  (String, String) -> IO()
writeXml tuples = do
                    writeFile (fst tuples ++ ".xml") "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
                    appendFile ( fst tuples ++ ".xml") (snd tuples)

-- parse method: http://hackage.haskell.org/packages/archive/parsec/latest/doc/html/Text-Parsec-Prim.html#v:parse
-- parse method type :: Stream s Identity t => Parsec s () a -> SourceName -> s -> Either ParseError a
-- no direct input
parseJSON :: String -> String
parseJSON str = case (parse fileContent "" str) of
               Left  s -> error (show s)
               -- just a security to see the error, returns to main
                          parseMain
               -- return the right value whitout error
               Right v ->  v

-- Structure of a json: of type {"Property":"Value", "Property":"value"}
-- the parsing will end up when the parser finishes to read successfully
fileContent :: Parser String
fileContent = do json <- pMObject
                 eof
                 return json

-- json type value
jsonValue :: Parser String
jsonValue = do obj <- currentparser(pMStringValue  <|> pMObject <|> patternMachingArray)
               return obj


 -- Pattern Maching Object
 -- where keyword: http://learnyouahaskell.com/syntax-in-functions
pMObject :: Parser String
pMObject = do tupleConvertedValues <- between first last (sepBy pMTuples separator)
              return (concat tupleConvertedValues)
              where
              first  = currentparser (char '{')
              last = currentparser (char '}')
              separator = currentparser(char ',')


-- JSON Tuples of type "property:value"
-- The value could be a string, a object or an array
-- return the corresponding xml string
pMTuples :: Parser String
pMTuples = do property   <- currentparser(pMStringValue)
              currentparser (char ':')
              value <- currentparser(jsonValue)
              return ("<" ++ property ++ ">" ++ value ++ "</" ++ property ++ ">")


-- JSON String
-- matching the name of the property or, value
pMStringValue :: Parser String
pMStringValue = do val <- between (char '"' ) (char '"' )  (many (letter <|> alphaNum))
                   return val

-- matching the content of an array, return the value in a string xml format.
patternMachingArray :: Parser String
patternMachingArray = do values <- between first last (sepBy (currentparser jsonValue) separator)
                         return (concat $ map (\x -> "<item>" ++ x ++ "</item>") values)
                        where
                        first  = currentparser (char '[')
                        last = currentparser (char ']')
                        separator = currentparser(char ',')

main = parseMain
