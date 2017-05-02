{-# LANGUAGE StandaloneDeriving #-}
module Main where
import Prelude hiding (Num)
import qualified Prelude (Num)
import Text.Megaparsec hiding (State, parse)
import qualified Text.Megaparsec (parse)
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Control.Monad (void)

main = undefined

--------------------------------------------------------------------------------

type Num = Integer
type Var = String

type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]

type Z = Integer
type T = Bool

type State = Var -> Z

data Aexp = N Num | V Var | Mult Aexp Aexp | Add Aexp Aexp
          | Sub Aexp Aexp
                    deriving (Show, Eq, Read)

data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp
          | Le Aexp Aexp | Eq Aexp Aexp
                    deriving (Show, Eq, Read)

data Stm = Skip | Ass Var Aexp | Comp Stm Stm | If Bexp Stm Stm
         | While Bexp Stm | Block DecV DecP Stm | Call Pname
                    deriving (Show, Eq, Read)

data Config = Inter Stm State | Final State
type EnvP_Dynamic = Pname -> Stm
newtype EnvP_Mixed = EnvP_Mixed {run :: Pname -> (Stm, EnvP_Mixed)}

--------------------------------------------------------------------------------
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
          <|> TRUE <$ (tok "TRUE" <|> tok "true" <|> tok "True")
          <|> FALSE <$ (tok "FALSE" <|> tok "false" <|> tok "False")
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
  <|> try (block)
  <|> try (ifStm)
  <|> try (whileStm)
  <|> try (Call <$> (tok "call" *> var))
  <|> Skip <$ (tok "SKIP" <|> tok "skip")
  <|> Ass <$> (var <* tok ":=") <*> arithmeticExp

num :: Parser Integer
num = read <$> (some (oneOf ['0' .. '9'])) <* whitespace

var :: Parser [Char]
var = some (oneOf ['A' .. 'Z'] <|> oneOf ['a' .. 'z']) <* whitespace

block :: Parser Stm
block = do
  tok "begin"
  declaredVar <- decV
  declaredProc <- decP
  statement <- stm
  tok "end"
  return (Block declaredVar declaredProc statement)

decV :: Parser [(Var, Aexp)]
decV = many(try decV' <|> try (parenthesis decV'))

decV' :: Parser (Var, Aexp)
decV' = do
  tok "var"
  variable <- var
  tok ":="
  expression <- arithmeticExp
  tok ";"
  return (variable, expression)

decP :: Parser [(Pname, Stm)]
decP = many(try decP')

decP' :: Parser (Pname, Stm)
decP' = do
  tok "proc"
  pname <- var
  tok "is"
  statement <- parenthesis stm
  tok ";"
  return (pname, statement)

tok :: String -> Parser String
tok t = string t <* whitespace

--whitespace :: Parser ()
--whitespace = many (void spaceChar) *> pure ()

parenthesis :: Parser a -> Parser a
parenthesis = between (tok "(")(tok ")")

procParser :: Parser Stm
procParser = whitespace >> stm

whitespace :: Parser ()
whitespace = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

parse :: String -> Stm
parse x = do
  case runParser procParser "" x of
    Right procParser -> procParser

parseFile :: FilePath -> IO ()
parseFile filePath = do
  file <- readFile filePath
  putStrLn $ case Text.Megaparsec.parse procParser filePath file of
    Left err   -> parseErrorPretty err
    Right procParser -> show procParser

--------------------------------------------------------------------------------
n_val :: Num -> Z
n_val x = x

s :: State
s x | x == "x"  = 3
    | x == "y"  = 2
    | x == "z"  = 4
    | otherwise = 0

a :: Aexp
a = Mult (Add (V "x") (V "y")) (Sub (V "z") (N 1))

a_val :: Aexp -> State -> Z
a_val (N x)      s = x
a_val (V x)      s = s x
a_val (Add x y)  s = a_val x s + a_val y s
a_val (Mult x y) s = a_val x s * a_val y s
a_val (Sub x y)  s = a_val x s - a_val y s

b :: Bexp
b = Neg (Eq (Add (V "x") (V "y")) (N 4))

b_val :: Bexp -> State -> T
b_val TRUE      s = True
b_val FALSE     s = False
b_val (Neg x)   s = not (b_val x s)
b_val (And x y) s = b_val x s && b_val y s
b_val (Eq x y)  s = a_val x s == a_val y s
b_val (Le x y)  s = a_val x s <= a_val y s

-------------------------------------------------------
--Dynamic natural Semantics
ns_stm_dynamic :: EnvP_Dynamic -> Config -> Config
ns_stm_dynamic envP (Inter (Skip) s) = Final s
ns_stm_dynamic envP (Inter (Ass x a) s) = Final s'
  where
    Final s' = Final(updateState_dynamic s (a_val a s) x)
ns_stm_dynamic envP (Inter (If b ss1 ss2) s)
  | b_val b s = Final s'
  | otherwise = Final s''
  where
    Final s' = ns_stm_dynamic envP (Inter ss1 s)
    Final s'' = ns_stm_dynamic envP (Inter ss2 s)
ns_stm_dynamic envP (Inter (Comp ss1 ss2) s) = Final s''
  where
    Final s'  = ns_stm_dynamic envP (Inter ss1 s)
    Final s'' = ns_stm_dynamic envP (Inter ss2 s')
ns_stm_dynamic envP (Inter (While b ss1) s)
  | b_val b s = Final s''
  | otherwise = Final s
  where
    Final s' = ns_stm_dynamic envP (Inter ss1 s)
    Final s'' = ns_stm_dynamic envP (Inter (While b ss1) s')
ns_stm_dynamic envP (Inter (Block decV decP stm) s) = Final s'''
  where
    s' = updateDecV s decV
    envP' = updateDecP_dynamic envP decP
    Final s'' = ns_stm_dynamic envP' (Inter stm s')
    s''' = (\var -> if (var `elem` (map fst decV)) then s var else s'' var )
ns_stm_dynamic envP (Inter (Call pname) s) = Final s'
  where
    Final s' = ns_stm_dynamic envP (Inter (envP pname) s)


--Mixed natural semantics
ns_stm_mixed :: EnvP_Mixed -> Config -> Config
ns_stm_mixed envP (Inter (Skip) s) = Final s
ns_stm_mixed envP (Inter (Ass x a) s) = Final s'
  where
    Final s' = Final(updateState_mixed s (a_val a s) x)
ns_stm_mixed envP (Inter (If b ss1 ss2) s)
  | b_val b s = Final s'
  | otherwise = Final s''
  where
    Final s' = ns_stm_mixed envP (Inter ss1 s)
    Final s'' = ns_stm_mixed envP (Inter ss2 s)
ns_stm_mixed envP (Inter (Comp ss1 ss2) s) = Final s''
  where
    Final s'  = ns_stm_mixed envP (Inter ss1 s)
    Final s'' = ns_stm_mixed envP (Inter ss2 s')
ns_stm_mixed envP (Inter (While b ss1) s)
  | b_val b s = Final s''
  | otherwise = Final s
  where
    Final s' = ns_stm_mixed envP (Inter ss1 s)
    Final s'' = ns_stm_mixed envP (Inter (While b ss1) s')
ns_stm_mixed envP (Inter (Block decV decP stm) s) = Final s'''
  where
    s' = updateDecV s decV
    envP' = updateDecP_mixed envP decP
    Final s'' = ns_stm_mixed envP' (Inter stm s')
    s''' = (\var -> if (var `elem` (map fst decV)) then s var else s'' var )
ns_stm_mixed envP (Inter (Call pname) s) = Final s'
  where
    (stm, envP') = run envP pname
    envP'' = updateDecP_mixed envP' [(pname, stm)]
    Final s' = ns_stm_mixed envP'' (Inter (stm) s)
--------------------------------------------------------
--Update functions
updateDecV :: State-> DecV -> State
updateDecV s decV = foldl updateDecV' s decV
  where
    updateDecV':: State -> (Var, Aexp) -> State
    updateDecV' s (var, aexp) = \var' -> case () of
            _ | var' == var -> a_val aexp s
              | otherwise   -> s var'

updateDecP_dynamic :: EnvP_Dynamic -> DecP -> EnvP_Dynamic
updateDecP_dynamic envP decP = foldl updateDecP_dynamic' envP decP
  where
    updateDecP_dynamic' :: EnvP_Dynamic -> (Pname, Stm) -> EnvP_Dynamic
    updateDecP_dynamic' envP (pname, stm) = \pname' -> case () of
            _ | pname' == pname -> stm
              | otherwise -> envP pname'

updateState_dynamic :: State -> Z -> Var -> State
updateState_dynamic s aexp var var' = if var == var' then aexp else s var'

updateDecP_mixed :: EnvP_Mixed -> DecP -> EnvP_Mixed
updateDecP_mixed envP decP = foldl updateDecP_mixed' envP decP
  where
    updateDecP_mixed' :: EnvP_Mixed -> (Pname, Stm) -> EnvP_Mixed
    updateDecP_mixed' envP (pname, stm) = EnvP_Mixed(\pname' -> case () of
                      _ | pname' == pname -> (stm, envP)
                        | otherwise -> run envP pname')

updateState_mixed :: State -> Z -> Var -> State
updateState_mixed s aexp var var' = if var == var' then aexp else s var'

-------------------------------------------------------
--State and Environment
state :: State
state = \x -> case () of
  _ | x == "x" -> 5 | otherwise -> 0

environment_dynamic :: EnvP_Dynamic
environment_dynamic = \pname -> undefined

environment_mixed :: EnvP_Mixed
environment_mixed = EnvP_Mixed(\pname -> undefined)

-------------------------------------------------------

s_dynamic :: Stm -> State -> State
s_dynamic prog initState = finalState
  where
    Final finalState = ns_stm_dynamic environment_dynamic (Inter prog initState)

s_mixed :: Stm -> State -> State
s_mixed prog initState = finalState
  where
    Final finalState = ns_stm_mixed environment_mixed (Inter prog initState)

---------------------------------------------------------
--Program
prog1 = (Comp (Ass "y" (N 50)) (Ass "z" (Mult (V "y") (N 2))))
progFac = Block [] [("fac",Block [("z",V "x")] [] (If (Eq (V "x") (N 1)) Skip (Comp (Ass "x" (Sub (V "x") (N 1))) (Comp (Call "fac") (Ass "y" (Mult (V "z") (V "y")))))))] (Comp (Ass "y" (N 1)) (Call "fac"))
progScope_Test = Block [("x",N 0)] [("p",Ass "x" (Mult (V "x") (N 2))),("q",Call "p")] (Block [("x",N 5)] [("p",Ass "x" (Add (V "x") (N 1)))] (Comp (Call "q") (Ass "y" (V "x"))))
