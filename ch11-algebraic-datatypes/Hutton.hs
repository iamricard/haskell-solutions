module Hutton where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit literal) = literal
eval (Add expr expr') = eval expr + eval expr'

printExpr :: Expr -> String
printExpr (Lit literal) = show literal
printExpr (Add expr expr') = printExpr expr ++ " + " ++ printExpr expr'
