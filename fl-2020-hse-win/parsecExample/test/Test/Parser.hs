module Test.Parser where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Data.Either (isLeft)

import Parser

unit_success :: Assertion
unit_success = do
  parseString "13"        @?= Right (Num 13)
  parseString "42"        @?= Right (Num 42)
  parseString "007"       @?= Right (Num 7)
  parseString "a"         @?= Right (Ident "a")
  parseString "(a+13)*42" @?= Right (Mult (Plus (Ident "a") (Num 13)) (Num 42))
  parseString "a+13*42"   @?= Right (Plus (Ident "a") (Mult (Num 13) (Num 42)))
  parseString "1^2^3^4"   @?= Right (Pow (Num 1) (Pow (Num 2) (Pow (Num 3) (Num 4))))
  parseString "a+2^3*4"   @?= Right (Plus (Ident "a") (Mult (Pow (Num 2) (Num 3)) (Num 4)))

assertLeft :: (Show a, Show b) => Either a b -> Assertion
assertLeft x =
  assertBool ("expected: Left\n but got: " ++ show x) (isLeft x)

unit_failure :: Assertion
unit_failure = do
  assertLeft $ parseString "a+2+"
  assertLeft $ parseString ""
  assertLeft $ parseString "+3"
  assertLeft $ parseString "1-3"
  assertLeft $ parseString "123abc"