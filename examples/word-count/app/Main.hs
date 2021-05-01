{-# LANGUAGE DataKinds #-}
module Main where

import           Pipeline

generateWords
  :: Circuit
       '[VariableStore]
       '[()]
       '[VariableStore ()]
       '[FileStore]
       '[[String]]
       '[FileStore [String]]
       N1
generateWords = functionTask (\_ -> ["apple", "banana", "grapefruit"]) (FileStore "fruit.txt")

newtype CommaSepFile a = CommaSepFile String


countLetters
  :: Circuit
       '[CommaSepFile]
       '[[String]]
       '[CommaSepFile [String]]
       '[FileStore]
       '[[String]]
       '[FileStore [String]]
       N1
countLetters = functionTask (foldr f []) (FileStore "count.txt")
  where f word cs = (concat [word, ":", show (length word)]) : cs

circuit
  :: Circuit
       '[VariableStore]
       '[()]
       '[VariableStore ()]
       '[FileStore]
       '[[String]]
       '[FileStore [String]]
       N1
circuit = generateWords <-> countLetters

main :: IO ()
main = putStrLn "Hello, Haskell!"
