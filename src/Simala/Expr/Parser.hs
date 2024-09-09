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
  (lexeme $ try $ do
    x <- identifier
    guard (x == k)
    pure x
  ) <?> show (Text.unpack k)

identifier :: Parser Text
identifier =
  Text.cons <$> satisfy isAlpha <*> takeWhileP (Just "identifier char") (\ x -> isAlphaNum x || x `elem` ("_" :: String))

name :: Parser Name
name =
  (quotedName <|> simpleName) <?> "identifier"

-- | A plain identifier.
simpleName :: Parser Name
simpleName =
  lexeme $ try $ do
    x <- identifier
    guard (x `notElem` keywords)
    pure x

-- | A quoted identifier between backticks.
quotedName :: Parser Name
quotedName =
  lexeme $ do
    void (char '`')
    x <- takeWhile1P (Just "printable char except backticks") (\ x -> isPrint x && not (x `elem` ("`" :: String)))
    void (char '`')
    pure x


keywords :: Set Text
keywords =
  Set.fromList
    [ "false"
    , "true"
    , "let"
    , "letrec"
    , "in"
    , "if"
    , "then"
    , "else"
    , "fun"
    , "undefined"
    , "opaque"
    , "transparent"
    , "rec"
    ]

expr :: Parser Expr
expr =
  makeExprParser
    baseExpr
    operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ postfix -- we allow mixed chains of function applications and record projections
        (   flip Project <$> (symbol "." *> name)
        <|> flip mkApp <$> argsOf expr
        )
    ]
    -- 7
  , [ binaryl "*" (builtin2 Product)
    , binaryl "/" (builtin2 Divide)
    , binaryl "%" (builtin2 Modulo)
    ]
    -- 6
  , [ binaryl "+" (builtin2 Sum)
    , binaryl "-" (builtin2 Minus)
    ]
    -- 5
  , [ binaryr ":" Cons
    ]
    -- 4
  , [ binaryl ">=" (builtin2 Ge)
    , binaryl "<=" (builtin2 Le)
    , binaryl ">" (builtin2 Gt)
    , binaryl "<" (builtin2 Lt)
    , binaryl "==" (builtin2 Eq)
    , binaryl "/=" (builtin2 Ne)
    ]
    -- 3
  , [ binaryr "&&" (builtin2 And)
    ]
    -- 2
  , [ binaryr "||" (builtin2 Or)
    ]
  -- , [ prefix (Fun Transparent <$ keyword "fun" <*> (argsOf name) <* symbol "=>")
  --   ]
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
      mkLet <$ keyword "let" <*> decls <* keyword "in" <*> expr
  <|> Fun <$ keyword "fun" <*> transparency <*> argsOf name <* symbol "=>" <*> expr
  <|> mkIfThenElse <$ keyword "if" <*> expr <* keyword "then" <*> expr <* keyword "else" <*> expr
  <|> List <$> between (symbol "[") (symbol "]") (sepBy expr (symbol ","))
  <|> Record <$> between (symbol "{") (symbol "}") (row (symbol "=") expr)
  <|> Lit <$> lit
  <|> Var <$> name
  <|> Atom <$ char '\'' <*> name
  <|> Undefined <$ keyword "undefined"
  <|> parens expr

-- | Parse a row of bindings. Parameterised over the
-- parser separating names from payloads, and the payload
-- parser.
--
row :: Parser sep -> Parser a -> Parser (Row a)
row sep payload =
  sepBy item (symbol ",")
  where
    item = (,) <$> name <* sep <*> payload

-- | Parse optional transparency. Missing transparency
-- is assumed to be 'Transparnet'.
--
transparency :: Parser Transparency
transparency =
  option Transparent
    (   Transparent <$ keyword "transparent"
    <|> Opaque      <$ keyword "opaque"
    )

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
    , ("foldr", Foldr)
    , ("foldl", Foldl)
    , ("case", Case)
    , ("sum", Sum)
    , ("product", Product)
    , ("merge", Merge)
    ]

decl :: Parser Decl
decl =
      NonRec <$> transparency <*> name <* symbol "=" <*> expr
  <|> Rec    <$  keyword "rec" <*> transparency <*> name <* symbol "=" <*> expr

decls :: Parser [Decl]
decls =
  sepBy decl (symbol ";")

instruction :: Parser Instruction
instruction =
      Declare                             <$> try decl
  <|> Eval                                <$> expr
  <|> ReplCommand (SetTrace TraceFull)    <$  (symbol ":trace" <|> symbol ":t")
  <|> ReplCommand (SetTrace TraceResults) <$  (symbol ":results" <|> symbol ":r")
  <|> ReplCommand (SetTrace TraceOff)     <$  (symbol ":notrace" <|> symbol ":n")
  <|> ReplCommand Quit                    <$  (symbol ":quit" <|> symbol ":q")
  <|> ReplCommand Help                    <$  (symbol ":help" <|> symbol ":h")
  <|> Noop                                <$  pure ()

execParser :: Parser a -> String -> Text -> Either String a
execParser p f input =
  case runParser (spaces *> p <* eof) f input of
    Right e  -> Right e
    Left err -> Left (errorBundlePretty err)

-- | Entry point for the expression parser.
parseExpr :: String -> Text -> Either String Expr
parseExpr =
  execParser expr

-- | Entry point for the declarations parser.
parseDecls :: String -> Text -> Either String [Decl]
parseDecls =
  execParser decls

-- | Entry point for the repl parser.
parseInstruction :: Text -> Either String Instruction
parseInstruction =
  execParser instruction "interactive"
