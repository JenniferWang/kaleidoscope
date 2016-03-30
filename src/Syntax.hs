module Syntax where

type Name = String

-- In Kaleidoscope, we have expressions and a function object.
data Expr = Float Double
          | BinOp Op Expr Expr
          | Var String
          | Call Name [Expr]
          | Function Name [Expr] Expr
          | Extern Name [Expr]
          deriving (Eq, Ord, Show)

data Op = Plus
        | Minus
        | Times
        | Divide
        deriving (Eq, Ord, Show)


