module Main where

import Combinators

testParser :: Show a => Parser a -> String -> IO ()
testParser parser str =
  print $ runParser parser str

main :: IO ()
main = do
  putStrLn ""
  testParser parseSum "123"
  testParser parseSum "abc"
  testParser parseSum "x+1*2+y"
