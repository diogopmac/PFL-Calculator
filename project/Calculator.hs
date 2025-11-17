{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char

type Name = String
type Env = [(Name, Integer)]

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Var Name
          deriving Show

--
-- a data type for commands
-- made up from letters, integer numbers, and expressions
--
data Comm = Assign Name Expr
          | Evaluate Expr

-- a recursive evaluator for expressions
--
eval :: Env -> Expr -> Integer
eval env (Num n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2
eval env (Mod e1 e2) = eval env e1 `mod` eval env e2
eval [] (Var name) = error "not found"
eval (env:envs) (Var name)
  | fst env == name = snd env
  | otherwise =  eval envs (Var name)

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
               <|>
               do char '-'
                  t <- term
                  exprCont (Sub acc t)
               <|> return acc

term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do char '*'
                   f <- factor
                   termCont (Mul acc f)
                <|>
                do char '/'
                   f <- factor
                   termCont (Div acc f)
                <|>
                do char '%'
                   f <- factor
                   termCont (Mod acc f)
                <|> return acc

factor :: Parser Expr
factor = do a <- variable
            return (Var a)
         <|>
         do n <- natural
            return (Num n)
         <|>
         do char '('
            e <- expr
            char ')'
            return e

command :: Parser Comm
command = do a <- variable 
             char '='
             e <- expr
             return (Assign a e)
          <|>
          do e <- expr
             return (Evaluate e)



natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser String
variable = do xs <- many1 (satisfy isAlpha)
              return xs

----------------------------------------------------------------             

main :: IO ()
main
  = do txt <- getContents
       calculator [] (lines txt)

-- | read-eval-print loop
calculator :: Env -> [String] -> IO ()
calculator env []  = return ()
calculator env (l:ls) = do let (result, newEnv) = execute env l
                           putStrLn result
                           calculator newEnv ls

-- | execute command or regular expression
execute :: Env -> String -> (String, Env)
execute env txt 
  = case parse command txt of
    [ (tree, "")] -> case tree of
                        Assign name e -> let val = eval env e
                                             newEnv = (name, val) : env
                                         in (show val, newEnv)
                        Evaluate e -> (show (eval env e), env)
    _ -> ("parse error", env) 