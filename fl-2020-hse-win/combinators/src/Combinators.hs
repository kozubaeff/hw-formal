module Combinators where

import Prelude hiding (seq, fmap)

-- data Either a b = Left a | Right b

-- data Maybe a = Nothing | Just a

newtype Parser a =
  Parser { runParser :: String -> Either String (a, String) }

char :: Char -> Parser Char
char c = Parser $ \inp ->
  case inp of
    (h : t) | h == c -> Right (c, t)
    (h : _) -> Left ("Unexpected symbol " ++ [h])
    [] -> Left "Unexpected EOL"

alt :: Parser a -> Parser a -> Parser a
alt p q = Parser $ \inp ->
  case runParser p inp of
    Right x -> Right x
    Left _ -> runParser q inp

digit :: Parser Char
digit = foldl1 alt $ map char "0123456789"

alpha :: Parser Char
alpha = foldl1 alt $ map char "abcdefghijklmnopqrstuvwxyz"

alphaNum :: Parser Char
alphaNum = digit `alt` alpha

-- E -> T + E | T

seq :: Parser a -> (a -> Parser b) -> Parser b
seq p f = Parser $ \inp ->
  case runParser p inp of
    Left e -> Left e
    Right (res, inp') -> runParser (f res) inp'

ret :: a -> Parser a
ret x = Parser $ \inp -> Right (x, inp)

cell :: Parser String
cell =
  (foldl1 alt $ map char "abcdefgh") `seq` \a ->
  (foldl1 alt $ map char "12345678") `seq` \b ->
  ret [a, b]

-- *
many :: Parser a -> Parser [a]
many p = Parser $ \inp ->
  case runParser p inp of
    Left _ -> Right ([], inp)
    Right (x, inp') ->
      case runParser (many p) inp' of
        Left _ -> Right ([x], inp')
        Right (xs, inp'') -> Right ((x:xs), inp'')

-- -- +: 1 or more repetitions
-- -- p+ == p `seq` p*
-- many1 :: Parser a -> Parser [a]
-- many1 p =
--   p `seq` \h ->
--   many p `seq` \t ->
--   ret (h : t)

many1 :: Parser a -> Parser [a]
many1 p =
  p `seq` \h ->
  fmap (h:) $ many p

fmap :: (a -> b) -> Parser a -> Parser b
fmap f p = Parser $ \inp ->
  case runParser p inp of
    Right (res, inp') -> Right (f res, inp')
    Left e -> Left e


-- I -> digit I | digit
-- I -> digit+

integer :: Parser Int
integer = fmap read $ many1 digit

ident :: Parser String
ident =
  alpha `alt` char '_' `seq` \h ->
  fmap (h:) $ many (alphaNum `alt` char '_')

data Expr = Num Int
          | Ident String
          | Plus Expr Expr
          | Mult Expr Expr
          deriving (Show, Eq)

parseFactor :: Parser Expr
parseFactor =
  (fmap Num integer) `alt` (fmap Ident ident)

list :: Parser elem -> Parser sep -> Parser [elem]
list elem sep =
  elem `seq` \h ->
  fmap (h:) $ (many (sep `seq` \_ -> elem))

-- M -> M * F | F
-- M -> F * F * F * ... * F
-- M -> (F *)^* F
parseMult :: Parser Expr
parseMult =
  foldl1 Mult `fmap` list parseFactor (char '*')

parseSum :: Parser Expr
parseSum =
  foldl1 Plus `fmap` list parseMult (char '+')
