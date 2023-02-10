module LambdaDC.Syntax () where

type Name = String

data Value
  = Var Name
  | Lam Name Expr

data Expr
  = Val Value
  | App Expr Expr
