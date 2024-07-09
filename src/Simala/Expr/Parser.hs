module Simala.Expr.Parser where

import Base
import qualified Base.Map as Map
import qualified Base.Set as Set
import qualified Base.Text as Text
import Simala.Expr.Type

import Control.Monad.Combinators.Expr
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

spaces :: Parser ()
spaces =
  Lexer.space
    space1
    (Lexer.skipLineComment "--")
    (Lexer.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme =
  Lexer.lexeme spaces

symbol :: Text -> Parser Text
symbol =
  Lexer.symbol spaces

keyword :: Text -> Parser Text
keyword k =
  lexeme $ try $ do
    x <- identifier
    guard (x == k)
    pure x

identifier :: Parser Text
identifier =
  Text.cons <$> satisfy isAlpha <*> takeWhileP (Just "alphanum") isAlphaNum

name :: Parser Name
name =
  lexeme $ try $ do
    x <- identifier
    guard (x `notElem` keywords)
    pure x

keywords :: Set Text
keywords =
  Set.fromList
    [ "false"
    , "true"
    , "let"
    , "in"
    , "if"
    , "then"
    , "else"
    , "fun"
    , "undefined"
    ]

expr :: Parser Expr
expr =
  makeExprParser
    baseExpr
    operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ postfix (flip mkApp <$> argsOf expr)
    ]
  , [ binaryl "*" (builtin2 Product)
    , binaryl "/" (builtin2 Divide)
    , binaryl "%" (builtin2 Modulo)
    ]
  , [ binaryl "+" (builtin2 Sum)
    ]
  , [ binaryr ":" Cons
    ]
  , [ binaryl ">=" (builtin2 Ge)
    , binaryl "<=" (builtin2 Le)
    , binaryl ">" (builtin2 Gt)
    , binaryl "<" (builtin2 Lt)
    , binaryl "==" (builtin2 Eq)
    , binaryl "/=" (builtin2 Ne)
    ]
  , [ prefix (Fun Transparent <$ keyword "fun" <*> (argsOf name) <* symbol "=>")
    ]
  ]

prefix :: Parser (Expr -> Expr) -> Operator Parser Expr
prefix p = Prefix (foldr (.) id <$> some p)

postfix :: Parser (Expr -> Expr) -> Operator Parser Expr
postfix p = Postfix (foldr (flip (.)) id <$> some p)

binaryl :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryl op f =
  InfixL (f <$ symbol op)

binaryr :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryr op f =
  InfixR (f <$ symbol op)

builtin2 :: Builtin -> Expr -> Expr -> Expr
builtin2 f e1 e2 = Builtin f [e1, e2]

baseExpr :: Parser Expr
baseExpr =
      Let Transparent <$ keyword "let" <*> name <* symbol "=" <*> expr <* keyword "in" <*> expr
  <|> IfThenElse <$ keyword "if" <*> expr <* keyword "then" <*> expr <* keyword "else" <*> expr
  <|> List <$> between (symbol "[") (symbol "]") (sepBy expr (symbol ","))
  <|> Lit <$> lit
  <|> Var <$> name
  <|> Undefined <$ keyword "undefined"
  <|> parens expr

argsOf :: Parser a -> Parser [a]
argsOf p = between (symbol "(") (symbol ")") (sepBy p (symbol ","))

lit :: Parser Lit
lit =
      IntLit <$> int
  <|> BoolLit False <$ keyword "false"
  <|> BoolLit True  <$ keyword "true"

int :: Parser Int
int =
  lexeme (negate <$ string "-" <*> Lexer.decimal <|> Lexer.decimal)

parens :: Parser a -> Parser a
parens =
  between (symbol "(") (symbol ")")

mkApp :: Expr -> [Expr] -> Expr
mkApp e@(Var n) es =
  case Map.lookup n builtins of
    Just b  -> Builtin b es
    Nothing -> App e es
mkApp e es = App e es

builtins :: Map Name Builtin
builtins =
  Map.fromList
    [ ("minimum", Minimum)
    , ("maximum", Maximum)
    , ("and", And)
    , ("or", Or)
    , ("not", Not)
    , ("fold", Fold)
    ]
