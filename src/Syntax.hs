module Syntax where

type Name = String

-- In Kaleidoscope, we have expressions and a function object.
data Expr = Float Double
          | UnaryOp Name Expr
          | BinaryOp Name Expr Expr
          | Var String
          | Call Name [Expr]
          | Function Name [Name] Expr
          | Extern Name [Name]
          deriving (Eq, Ord, Show)

-- data Op = Plus
--         | Minus
--         | Times
--         | Divide
--         | Assign
--         | LT
--         deriving (Eq, Ord, Show)


