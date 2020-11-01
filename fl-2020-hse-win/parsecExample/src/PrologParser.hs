module PrologParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char (isLower, isUpper)
import PrologAst


languageDef =
  emptyDef { Token.identStart = lower
           , Token.identLetter = alphaNum <|> char '_'
           , Token.reservedNames = ["module", "type"]
           , Token.reservedOpNames = [",", ";", "->", ":-"]
           }

lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = do
  i <- Token.identifier lexer
  guard $ isLower $ head i
  return i

var :: Parser [Char]
var = do
  h <- upper
  t <- many (alphaNum <|> char '_')
  spaces
  return (h:t)

whiteSpace = Token.whiteSpace lexer
reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer
brackets = Token.parens lexer
dot = Token.dot lexer
corkscrew = reservedOp ":-"
resComma = reservedOp ","
resSemi = reservedOp ";"
resArrow = reservedOp "->"

parseString = parse (do r <- prog; eof; return r) ""


-- nil = Atom "nil" []
-- cons x y = Atom "cons" [x, y]

manyBrackets par = brackets (manyBrackets par) <|> par

atom :: Parser Atom
atom = do
  head <- identifier
  args <- many atomHelper
  return $ Atom head args


atomHelper =  fmap Left (do head <- identifier; return  ( Atom head [])) <|>
              fmap Right var <|>
              manyBrackets (fmap Left atom <|>  fmap Right var)

relation :: Parser Relation
relation = do
  head <- atom
  body <- relationHelper
  dot
  return  $ Relation head body

sepList x sep = do h <- x; t <- many (sep >> x); return $ h:t

relationHelper = fmap Just (do corkscrew; disj) <|> return Nothing where
  expr          = fmap RAtom atom <|> brackets disj
  conj          = fmap (foldr1 Conj) $ sepList expr $ resComma
  disj          = fmap (foldr1 Disj) $ sepBy1 conj $ resSemi


parseModule :: Parser String
parseModule = do
  spaces
  reserved "module"
  name <- identifier
  dot
  return name

typeExpr :: Parser Type
typeExpr = fmap (foldr1 Arrow) $ sepList typeHelper resArrow

typeHelper = fmap TAtom atom <|> fmap Var var <|> brackets typeExpr

typ :: Parser TypeDef
typ = do
  reserved "type"
  name <- identifier
  te <- typeExpr
  dot
  return $ TypeDef name te

prog :: Parser PrologProgram
prog = try withmod <|> womod where
  withmod = do
    md <- parseModule
    tps  <- many typ
    rs   <- many relation
    return $ Program (Just md) tps rs
  womod   = do
    tps  <- many typ
    rs   <- many relation
    return $ Program Nothing tps rs
