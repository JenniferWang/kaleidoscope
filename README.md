### Implementing a JIT Compiled Language with Haskell and LLVM
Following the [tutorial](http://www.stephendiehl.com/llvm) by Stephen Diehl

### Setup for Mac OS
* install llvm `brew install homebrew/versions/llvm35`
* in order for GHC to find the library, export the path of llvm to `PATH`, mine is `/usr/local/Cellar/llvm35/3.5.1/bin`

### Notes
##### Abstract Syntax Tree and Concrete Syntax Tree
[Here](./asset/AST_CST.pdf) is a good explanation on the comparison between these two concepts. [Another](./assert/AST_CST2.pdf).

AST is usually the last product of the front-end of a compiler which captures the semantics of the source code while CST is usually not built explicitly and captures syntactic meaning, say `;` is the end of a statement. 

In `src/Syntax.hs`, we define the AST as follows:

```haskell
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
```

while in `src/Parser.hs`, we **parse** the source code and build the AST directly as follows:

```haskell
table = [ [ binary "*" Times Ex.AssocLeft, binary "/" Divide Ex.AssocLeft]
        , [ binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
        ]
...
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

```

These two code snippets actually demonstrate the difference between AST and CST. 
* In AST, there are no intermediate categories (factor, definition, precedence of operators) in order to build the tree unambiguously. 
* CST is specific to the syntax of the programming language. In this toy language, we specify that "+" is an infix operator; however, it could be prefix in another language. As this has nothing to do with the semantics, we only need a `BinOp` category in AST. 
* We need AST for the compiler backend, say type checking.
