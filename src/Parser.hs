module Parser where

import Text.Parsec
import Text.Parsec.String(Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

table = [ [ binary "*" Times Ex.AssocLeft, binary "/" Divide Ex.AssocLeft]
        , [ binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
        ]

int :: Parser Expr
int = integer >>= return . Float . fromInteger

floating :: Parser Expr
floating = float >>= return . Float

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

variable :: Parser Expr
variable = identifier >>= return . Var

function :: Parser Expr
function = do
  reserved "def" -- TODO:duplicated?
  name <- identifier
  args <- parens $ many variable
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

{-
  Context Free Grammar of the language

  Expr -> Factor '+' Factor | Factor '-' Factor
        | Factor '*' Factor | Factor '/' Factor (with higher precedence)

  Factor -> floating | int | Extern | Function | Call | variable
          | (Expr)

  Defn -> Extern | Function
        | Expr

  Toplevel -> Defn Toplevel | epsilon
-}

factor :: Parser Expr
factor =  try floating
      <|> try int
      <|> try extern
      <|> try function
      <|> try call
      <|> try variable
      <|> parens expr

defn :: Parser Expr
defn =  try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
  def <- defn
  reservedOp ";"
  return def

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"

