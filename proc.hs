{-# LANGUAGE StandaloneDeriving #-}
module Proc where
import Prelude hiding (Num)
import qualified Prelude (Num)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Data.List (intercalate)
import Control.Monad (void)

type Num = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]
data Aexp = N Num | V Var | Mult Aexp Aexp | Add Aexp Aexp | Sub Aexp Aexp
data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp | Le Aexp Aexp | Eq Aexp Aexp
data Stm = Skip | Ass Var Aexp | Comp Stm Stm | If Bexp Stm Stm | While Bexp Stm | Block DecV DecP Stm | Call Pname

arithmeticExp :: Parser Aexp
arithmeticExp = try (aExp)

aExp :: Parser Aexp
aExp = makeExprParser arithmeticExp' arithmeticOperators

arithmeticExp' :: Parser Aexp
arithmeticExp' = try(parenthesis arithmeticExp)
             <|> N <$> num
             <|> V <$> var

arithmeticOperators :: [[Operator Parser Aexp]]
arithmeticOperators =
  [[ InfixL (Mult <$ tok "*")],
   [ InfixL (Add <$ tok "+"),
     InfixL (Sub <$ tok "-")]]

booleanExp :: Parser Bexp
booleanExp = try (bExp)

booleanExp' :: Parser Bexp
booleanExp' = try(parenthesis booleanExp)
          <|> TRUE <$ tok "TRUE"
          <|> FALSE <$ tok "FALSE"
          <|> try(Le <$> arithmeticExp <*> (tok "<=" *> arithmeticExp))
          <|> try(Eq <$> arithmeticExp <*> (tok "=" *> arithmeticExp))

bExp :: Parser Bexp
bExp = makeExprParser booleanExp' booleanOperators

booleanOperators :: [[Operator Parser Bexp]]
booleanOperators =
  [[Prefix (Neg <$ tok "!")],
   [InfixL (And <$ tok "&")]]

stm :: Parser Stm
stm = try (comp)

comp :: Parser Stm
comp = makeExprParser stm' [[ InfixR (Comp <$ tok ";")]]

ifStm :: Parser Stm
ifStm = do
  tok "if"
  cond <- booleanExp
  tok "then"
  stm1 <- stm
  tok "else"
  stm2 <- stm
  return (If cond stm1 stm2)

whileStm :: Parser Stm
whileStm = do
  tok "while"
  cond <- booleanExp
  tok "do"
  stm1 <- stm
  return (While cond stm1)

stm' :: Parser Stm
stm' = try (parenthesis stm)
  <|> try (ifStm)
  <|> try (whileStm)
  <|> try (Call <$> (tok "call" *> var))
  <|> Skip <$ tok "SKIP"
  <|> Ass <$> (var <* tok ":=") <*> arithmeticExp

num :: Parser Integer
num = read <$> (some (oneOf ['0' .. '9'])) <* whitespace

var :: Parser [Char]
var = some (oneOf ['A' .. 'Z'] <|> oneOf ['a' .. 'z']) <* whitespace

tok :: String -> Parser String
tok t = string t <* whitespace

whitespace :: Parser ()
whitespace = many (void spaceChar) *> pure ()

parenthesis :: Parser a -> Parser a
parenthesis = between (tok "(")(tok ")")

deriving instance Show Aexp
deriving instance Show Bexp
deriving instance Show Stm
