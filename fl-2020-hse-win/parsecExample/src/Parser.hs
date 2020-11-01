module Parser where

import Text.ParserCombinators.Parsec

data Expr = Num Int
          | Ident String
          | Plus Expr Expr
          | Mult Expr Expr
          | Pow Expr Expr
          deriving (Show, Eq)

parseString :: String -> Either ParseError Expr
parseString =
  parse (do r <- exprParser; eof; return r) ""

-- cell =
--   letter `seq` \x ->
--   number `seq` \y ->
--   ret [x,y]

-- cell = do
--   x <- letter
--   y <- number
--   ret [x, y]

parseNum :: Parser Int
parseNum = do
  num <- many1 digit
  return (read num :: Int)

-- <|> == `alt`
parseIdent :: Parser String
parseIdent = do
  h <- (letter <|> char '_')
  t <- many (alphaNum <|> char '_')
  return (h:t)

parseFactor :: Parser Expr
parseFactor =
  fmap Num parseNum <|>
  fmap Ident parseIdent <|>
  (do
    _ <- char '('
    e <- parseExpr
    _ <- char ')'
    return e
  )

  -- (do
  --   char '('
  --   e <- parseExpr
  --   char ')'
  --   return e
  -- )

parseList elem sep = do
  h <- elem
  t <- many (sep >> elem)
  return (h:t)

parsePow =
  fmap (foldr1 Pow) $ parseList parseFactor (char '^')

parseMult :: Parser Expr
parseMult =
  fmap (foldl1 Mult) $ parseList parsePow (char '*')

parseExpr =
  fmap (foldl1 Plus) $ parseList parseMult (char '+')

exprParser =
  parseExpr
