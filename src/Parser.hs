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
int = Float <$> (fromInteger <$> integer)

floating :: Parser Expr
floating = Float <$> float

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = reserved "def"                         -- TODO: duplicated?
        *> (Function <$> identifier               -- name
                     <*> parens (many variable)   -- arguments
                     <*> expr)                    -- body

extern :: Parser Expr
extern = reserved "extern"
      *> (Extern <$> identifier                   -- name
                 <*> (parens $ many variable))    -- arguments

call :: Parser Expr
call = Call <$> identifier <*> (parens $ commaSep expr)

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
contents p = Tok.whiteSpace lexer *> p <* eof

toplevel :: Parser [Expr]
toplevel = many $ defn <* reservedOp ";"

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"

