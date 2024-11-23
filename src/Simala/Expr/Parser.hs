module Simala.Expr.Parser where

import Base
import qualified Base.Map as Map
import qualified Base.Set as Set
import qualified Base.Text as Text
import Simala.Expr.Lexer
import Simala.Expr.Type

import Control.Monad.Combinators.Expr
import Optics
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void TokenStream

spaces :: Parser ()
spaces =
  void (takeWhileP (Just "space token") isSpaceToken)

isSpaceToken :: PosToken -> Bool
isSpaceToken t =
  case t.payload of
    WhiteSpace _   -> True
    LineComment _  -> True
    BlockComment _ -> True
    _              -> False

lexeme :: Parser a -> Parser a
lexeme =
  Lexer.lexeme spaces

-- | A token parser for a token type that has no flexible content, plus subsequent whitespace.
stok :: TokenType -> Parser ()
stok tt =
  lexeme (stok' tt)

-- | A token parser for a token type that has no flexible content, no whitespace.
stok' :: TokenType -> Parser ()
stok' tt =
  token
    (\ t -> if t.payload == tt then Just () else Nothing)
    (Set.singleton (Tokens (trivialToken tt :| [])))

trivialToken :: TokenType -> PosToken
trivialToken tt =
  MkPosToken trivialRange tt
  where
    trivialRange :: SrcRange
    trivialRange = MkSrcRange trivialPos trivialPos 0

    trivialPos :: SrcPos
    trivialPos = MkSrcPos "" 0 0

tok :: (TokenType -> Maybe a) -> String -> Parser a
tok p lbl =
  lexeme
    (token
      (\ t -> p t.payload)
      Set.empty
    )
  <?> lbl

quotedName :: Parser Name
quotedName =
  tok (preview #_Quoted) "quoted identifier"

simpleName :: Parser Name
simpleName =
  tok (preview #_Identifier) "simple identifier"

name :: Parser Name
name =
  (quotedName <|> simpleName) <?> "identifier"

fracLit :: Parser Double
fracLit =
  tok (preview #_TFracLit) "fractional literal"

intLit :: Parser Int
intLit =
  tok (preview #_TIntLit) "integer literal"

stringLit :: Parser Text
stringLit =
  tok (preview #_TStringLit) "string literal"

sdirective :: Text -> Parser ()
sdirective d =
  lexeme (void (stok' (Directive d)))

sidentifier :: Text -> Parser ()
sidentifier n =
  lexeme (void (stok' (Identifier n)))

expr :: Parser Expr
expr =
  makeExprParser
    baseExpr
    operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ postfix -- we allow mixed chains of function applications and record projections
        (   flip Project <$> (stok Dot *> name)
        <|> flip mkApp <$> argsOf expr
        )
    ]
    -- 7
  , [ binaryl Times (builtin2 Product)
    , binaryl Divides (builtin2 Divide)
    , binaryl Percent (builtin2 Modulo)
    ]
    -- 6
  , [ binaryl Plus (builtin2 Sum)
    , binaryl TMinus (builtin2 Minus)
    , binaryr PlusPlus (builtin2 Append) -- unclear whether it should be 5 ((:) in Haskell) or 6 ((<>) in Haskell), unclear if right-associative helps
      -- NOTE: I've chosen 6 over 5 because it is easier to handle with lookahead / try due to both + and ++ being operators
    ]
    -- 5
  , [ binaryr Colon mkCons
    ]
    -- 4
  , [ binaryl GreaterEquals (builtin2 Ge)
    , binaryl LessEquals (builtin2 Le)
    , binaryl GreaterThan (builtin2 Gt)
    , binaryl LessThan (builtin2 Lt)
    , binaryl EqualsEquals (builtin2 Eq)
    , binaryl NotEquals (builtin2 Ne)
    , binaryl ApproxEquals (builtin2 HEq)
    ]
    -- 3
  , [ binaryr TAnd (builtin2 And)
    ]
    -- 2
  , [ binaryr TOr (builtin2 Or)
    ]
  -- , [ prefix (Fun Transparent <$ keyword "fun" <*> (argsOf name) <* symbol "=>")
  --   ]
  ]

prefix :: Parser (Expr -> Expr) -> Operator Parser Expr
prefix p = Prefix (foldr (.) id <$> some p)

postfix :: Parser (Expr -> Expr) -> Operator Parser Expr
postfix p = Postfix (foldr (flip (.)) id <$> some p)

binaryl :: TokenType -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryl op f =
  InfixL (f <$ stok op)

binaryr :: TokenType -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryr op f =
  InfixR (f <$ stok op)

builtin2 :: Builtin -> Expr -> Expr -> Expr
builtin2 f e1 e2 = Builtin f [e1, e2]

baseExpr :: Parser Expr
baseExpr =
      mkLet <$ stok KLet <*> decls <* stok KIn <*> expr
  <|> Fun <$ stok KFun <*> transparency <*> argsOf name <* stok Implies <*> expr
  <|> mkIfThenElse <$ stok KIf <*> expr <* stok KThen <*> expr <* stok KElse <*> expr
  <|> mkList <$> between (stok SOpen) (stok SClose) (sepBy expr (stok Comma))
  <|> Record <$> between (stok COpen) (stok CClose) (row (stok Equals) expr)
  <|> Lit <$> lit
  <|> Var <$> name
  <|> Atom <$ stok' Quote <*> name
  <|> Undefined <$ stok KUndefined
  <|> parens expr

-- | Parse a row of bindings. Parameterised over the
-- parser separating names from payloads, and the payload
-- parser.
--
row :: Parser sep -> Parser a -> Parser (Row a)
row sep payload =
  sepBy item (stok Comma)
  where
    item = (,) <$> name <* sep <*> payload

-- | Parse optional transparency. Missing transparency
-- is assumed to be 'Transparnet'.
--
transparency :: Parser Transparency
transparency =
  option Transparent
    (   Transparent <$ stok KTransparent
    <|> Opaque      <$ stok KOpaque
    )

argsOf :: Parser a -> Parser [a]
argsOf p = between (stok POpen) (stok PClose) (sepBy p (stok Comma))

lit :: Parser Lit
lit =
      FracLit <$> fracLit
  <|> IntLit <$> intLit
  <|> BoolLit False <$ stok KFalse
  <|> BoolLit True  <$ stok KTrue
  <|> StringLit <$> stringLit

parens :: Parser a -> Parser a
parens =
  between (stok POpen) (stok PClose)

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
    , ("floor", Floor)
    , ("ceiling", Ceiling)
    , ("fromInt", FromInt)
    , ("explode", Explode)
    , ("typeOf", TypeOf)
    ]

decl :: Parser Decl
decl =
      Eval   <$  sdirective "eval" <*> expr
  <|> NonRec <$> transparency <*> name <* stok Equals <*> expr
  <|> Rec    <$  stok KRec <*> transparency <*> name <* stok Equals <*> expr

replEvalDecl :: Parser Decl
replEvalDecl = Eval <$> expr

decls :: Parser [Decl]
decls =
  sepEndBy decl (stok Semicolon)

replCommand :: Text -> Text -> Parser ()
replCommand tl ts =
  try (stok Colon *> (sidentifier tl <|> sidentifier ts))

instruction :: Parser Instruction
instruction =
      Declare                             <$> try decl
  <|> Declare                             <$> try replEvalDecl
  <|> ReplCommand (SetTrace TraceFull)    <$  replCommand "trace" "t"
  <|> ReplCommand (SetTrace TraceResults) <$  replCommand "results" "r"
  <|> ReplCommand (SetTrace TraceOff)     <$  replCommand "notrace" "n"
  <|> ReplCommand Quit                    <$  replCommand "quit" "q"
  <|> ReplCommand Help                    <$  replCommand "help" "h"
  <|> Noop                                <$  pure ()

execParser :: Parser a -> String -> Text -> Either String a
execParser p f input =
  case execLexer f input of
    Left err -> Left err
    Right ts ->
      case runParser (spaces *> p <* eof) f (MkTokenStream (Text.unpack input) ts) of
        Left err -> Left (errorBundlePretty err)
        Right x  -> Right x

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
