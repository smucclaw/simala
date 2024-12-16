{-# LANGUAGE LambdaCase #-}
module Simala.Expr.Parser where

import Base
import qualified Base.Map as Map
import qualified Base.Set as Set
import qualified Base.Text as Text
import Simala.Expr.Lexer
import Simala.Expr.Type

import Control.Arrow (second)
import Control.Monad.Combinators.Expr
import Optics
import Text.Megaparsec
import Simala.Expr.Metadata

type Parser = Parsec Void TokenStream

data WithWs a = WithWs [PosToken] a
  deriving stock Show
  deriving (Functor)

betweenHoles :: [a] -> [MetaElement] -> [MetaElement]
betweenHoles list = go
  where
  go [] = case null list of
    True -> []
    _ -> [MetaHole]
  go (x:xs) = MetaHole : x : go xs

fromWs :: WithWs PosToken -> MetaElement
fromWs (WithWs after posToken) = MetaCsn
  ClusterMeta
    { payload = mkConcreteSyntaxNode [posToken]
    , trailing = mkConcreteSyntaxNode after
    , label = tokenTypeLabel posToken.payload
    }

unWs :: WithWs a -> a
unWs (WithWs _ a) = a

instance Applicative WithWs where
  pure a = WithWs [] a
  WithWs ps f <*> WithWs ps2 x = WithWs (ps <> ps2) (f x)

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

lexeme :: Parser a -> Parser (WithWs a)
lexeme p = do
  a <- p
  ws <- many (satisfy isSpaceToken)
  pure $ WithWs ws a

-- | A token parser for a token type that has no flexible content, plus subsequent whitespace.
stok :: TokenType -> Parser (WithWs PosToken)
stok tt =
  lexeme (stok' tt)

-- | A token parser for a token type that has no flexible content, no whitespace.
stok' :: TokenType -> Parser PosToken
stok' tt =
  token
    (\ t -> if t.payload == tt then Just t else Nothing)
    (Set.singleton (Tokens (trivialToken tt :| [])))

trivialToken :: TokenType -> PosToken
trivialToken tt =
  MkPosToken trivialRange tt
  where
    trivialRange :: SrcRange
    trivialRange = MkSrcRange trivialPos trivialPos 0

    trivialPos :: SrcPos
    trivialPos = MkSrcPos "" 0 0

tok :: (TokenType -> Maybe a) -> String -> Parser (WithWs (PosToken, a))
tok p lbl =
  lexeme
    (token
      (\ t -> fmap (t,) $ p t.payload)
      Set.empty
    )
  <?> lbl

quotedName :: Parser (WithWs (PosToken, Name))
quotedName =
  tok (preview #_Quoted) "quoted identifier"

simpleName :: Parser (WithWs (PosToken, Name))
simpleName =
  tok (preview #_Identifier) "simple identifier"

name :: Parser (WithWs (PosToken, Name))
name =
  (quotedName <|> simpleName) <?> "identifier"

variable :: Parser Variable
variable = do
  WithWs ws (origToken, n) <- (quotedName <|> simpleName) <?> "identifier"
  pure $ Variable (mkMeta [fromWs $ WithWs ws origToken]) n

fracLit :: Parser (WithWs (PosToken, Double))
fracLit =
  tok (preview #_TFracLit) "fractional literal"

intLit :: Parser (WithWs (PosToken, Int))
intLit =
  tok (preview #_TIntLit) "integer literal"

stringLit :: Parser (WithWs (PosToken, Text))
stringLit =
  tok (preview #_TStringLit) "string literal"

sdirective :: Text -> Parser (WithWs PosToken)
sdirective d =
  stok (Directive d)

sidentifier :: Text -> Parser (WithWs PosToken)
sidentifier n =
  stok (Identifier n)

expr :: Parser Expr
expr =
  makeExprParser
    baseExpr
    operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ postfix -- we allow mixed chains of function applications and record projections
        (   recordProject
        <|> funAppOp
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

recordProject :: Parser (Expr -> Expr)
recordProject = do
  dotWs <- stok Dot
  var <- variable
  pure $ \e ->
    Project
      (mkMeta [mkHoleWithType e, fromWs dotWs, mkHoleWithType var])
      e
      var

funAppOp :: Parser (Expr -> Expr)
funAppOp = do
  (open, (args, seps), close) <- argsOf expr
  pure $ \e ->
    mkApp
      -- Usually, `e` should be
      (mkMeta $ [fromWs open] <> betweenHoles args (fmap fromWs seps) <> [fromWs close])
      e
      args

prefix :: Parser (Expr -> Expr) -> Operator Parser Expr
prefix p = Prefix (foldr (.) id <$> some p)

postfix :: Parser (Expr -> Expr) -> Operator Parser Expr
postfix p = Postfix (foldr (flip (.)) id <$> some p)

binaryl :: TokenType -> (Meta -> Expr -> Expr -> Expr) -> Operator Parser Expr
binaryl op f =
  InfixL $ do
    opWs <- stok op
    pure $ f (mkMeta [mkHole, fromWs opWs, mkHole])

binaryr :: TokenType -> (Meta -> Expr -> Expr -> Expr) -> Operator Parser Expr
binaryr op f =
  InfixR $ do
    opWs <- stok op
    pure $ f (mkMeta [mkHole, fromWs opWs, mkHole])

builtin2 :: Builtin -> Meta -> Expr -> Expr -> Expr
builtin2 f m e1 e2 = Builtin m f [e1, e2]

baseExpr :: Parser Expr
baseExpr =
      letP
  <|> funP
  <|> ifThenElseP
  <|> listP -- mkList <$> between (stok SOpen) (stok SClose) (sepBy expr (stok Comma))
  <|> recordP -- Record <$> between (stok COpen) (stok CClose) (row (stok Equals) expr)
  <|> litP -- Lit <$> lit
  <|> varP -- Var <$> name
  <|> atomP -- Atom <$ stok' Quote <*> name
  <|> undefinedP -- Undefined <$ stok KUndefined
  <|> parensP-- parens expr

letP :: Parser Expr
letP = do
  l <- stok KLet
  ds <- decls
  inWs <- stok KIn
  e <- expr
  pure $ mkLet
    (mkMeta [fromWs l, mkHoleWithType ds, fromWs inWs, mkHoleWithType e])
    ds
    e

funP :: Parser Expr
funP = do
  funTok <- stok KFun
  transTok <- transparency
  (open, (vars, commas), close) <- argsOf variable
  impliesTok <- stok Implies
  e <- expr
  pure $ Fun
    (mkMeta $ mconcat
      [ [ fromWs funTok
        , fst transTok
        , fromWs open
        ]
      , betweenHoles vars (fmap fromWs commas)
      , [ fromWs close
        , fromWs impliesTok
        , mkHoleWithType e
        ]
      ]
    )
    (snd transTok)
    vars
    e

ifThenElseP :: Parser Expr
ifThenElseP = do
  ifTok <- stok KIf
  condExpr <- expr
  thenTok <- stok KThen
  thenExpr <- expr
  elseTok <- stok KElse
  elseExpr <- expr
  pure $ mkIfThenElse
    (mkMeta
      [ fromWs ifTok
      , mkHoleWithType condExpr
      , fromWs thenTok
      , mkHoleWithType thenTok
      , fromWs elseTok
      , mkHoleWithType elseExpr
      ]
    )
    condExpr
    thenExpr
    elseExpr

listP :: Parser Expr
listP = do
  (open, (items, commas), close) <- betweenP (stok SOpen) (stok SClose) (sepByP expr (stok Comma))
  pure $ mkList
    (mkMeta $ [fromWs open] <> betweenHoles items (fmap fromWs commas) <> [fromWs close])
    items

recordP :: Parser Expr
recordP = do
  (open, rowExprs, close) <- betweenP (stok COpen) (stok CClose) (rows (stok Equals) expr)
  pure $ Record
    (mkMeta [fromWs open, mkHoleWithType rowExprs, fromWs close])
    rowExprs

litP :: Parser Expr
litP = do
  l <- lit
  pure $ Lit
    (mkMeta [fromWs $ fmap fst l])
    (snd $ unWs l)

varP :: Parser Expr
varP = do
  n <- variable
  pure $ Var (mkMeta [mkHoleWithType n]) n

atomP :: Parser Expr
atomP = do
  quoteTok <- stok Quote
  n <- variable
  pure $ Atom (mkMeta [fromWs quoteTok, mkHoleWithType n]) n

undefinedP :: Parser Expr
undefinedP = do
  undefinedTok <- stok KUndefined
  pure $ Undefined (mkMeta [fromWs undefinedTok])

parensP :: Parser Expr
parensP = do
  (open, e, close) <- betweenP (stok POpen) (stok PClose) expr
  pure $ Parens
    (mkMeta [fromWs open, mkHoleWithType e, fromWs close])
    e

-- | Parse a row of bindings. Parameterised over the
-- parser separating names from payloads, and the payload
-- parser.
--
rows :: Parser (WithWs PosToken) -> Parser a -> Parser (Rows a)
rows sep payload = do
  (rowsA, commas) <- sepByP (row sep payload) (stok Comma)
  pure $ Rows
    (mkMeta $ betweenHoles rowsA (fmap fromWs commas))
    rowsA


row :: Parser (WithWs PosToken) -> Parser a -> Parser (Row a)
row sep payload = do
  var <- variable
  eqTok <- sep
  a <- payload
  pure $ Row
    (mkMeta [mkHoleWithType var, fromWs eqTok, mkHoleWithType a])
    var
    a

-- | Parse optional transparency. Missing transparency
-- is assumed to be 'Transparent'.
--
transparency :: Parser (MetaElement, Transparency)
transparency =
  option (mkCsn $ mkHiddenClusterMeta KTransparent, Transparent)
    (   ((,Transparent) . fromWs) <$> stok KTransparent
    <|> ((,Opaque) . fromWs)      <$> stok KOpaque
    )

argsOf :: Parser a -> Parser (WithWs PosToken, ([a], [WithWs PosToken]), WithWs PosToken)
argsOf p = betweenP (stok POpen) (stok PClose) (sepByP p (stok Comma))

lit :: Parser (WithWs (PosToken, Lit))
lit =
      fmap (second FracLit) <$> fracLit
  <|> fmap (second IntLit) <$> intLit
  <|> fmap (,BoolLit False) <$> stok KFalse
  <|> fmap (,BoolLit True)  <$> stok KTrue
  <|> fmap (second StringLit) <$> stringLit

mkApp :: Meta -> Expr -> [Expr] -> Expr
mkApp m e@(Var varMeta (Variable mInner n)) es =
  case Map.lookup n builtins of
    Just b  -> Builtin (mInner <> m) b es
    Nothing -> App (mkMeta [mkHoleWithType varMeta] <> m) e es
mkApp m e es = App (mkMeta [mkHole] <> m) e es

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
      evalDeclP
  <|> nonRecDeclP
  <|> recDeclP

evalDeclP :: Parser Decl
evalDeclP = do
  evalDirective <- sdirective "eval"
  e <- expr
  pure $ Eval (mkMeta [fromWs evalDirective, mkHoleWithType e]) e

nonRecDeclP :: Parser Decl
nonRecDeclP = do
  (tMeta, t) <- transparency
  var <- variable
  eqTok <- stok Equals
  e <- expr
  pure $ NonRec
    (mkMeta
      [ tMeta
      , mkHoleWithType var
      , fromWs eqTok
      , mkHoleWithType e
      ]
    )
    t
    var
    e

recDeclP :: Parser Decl
recDeclP = do
  r <- stok KRec
  (tMeta, t) <- transparency
  var <- variable
  eqTok <- stok Equals
  e <- expr
  pure $ Rec
    (mkMeta
      [ fromWs r
      , tMeta
      , mkHoleWithType var
      , fromWs eqTok
      , mkHoleWithType e
      ]
    )
    t
    var
    e

replEvalDecl :: Parser Decl
replEvalDecl = do
  e <- expr
  pure $ Eval emptyMeta e

decls :: Parser [Decl]
decls = do
  (ds, semicolons) <- sepEndByP decl (stok Semicolon)
  pure $ go ds semicolons
 where
  go :: [Decl] -> [WithWs PosToken] -> [Decl]
  go [] [] = []
  go [] (_:_) = error "decls: no decl but trailing whitespace. This is a parser bug."
  go [d] [] = [attachHiddenSemicolonToken d]
  go (d:rs) (w:ws) = attachTrailingWhiteSpace w d : go rs ws
  go (_:_) [] = error "decls: Ran out of semicolons. This is a parser bug."

replCommand :: Text -> Text -> Parser ()
replCommand tl ts =
  try (stok Colon *> (void $ sidentifier tl <|> sidentifier ts))

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

-- ----------------------------------------------------------------------------
-- AST manipulation utilities
-- ----------------------------------------------------------------------------

attachTrailingWhiteSpace :: WithWs PosToken -> Decl -> Decl
attachTrailingWhiteSpace tokWithWs = \case
  NonRec meta t n e -> NonRec (attach meta) t n e
  Rec meta t n e -> Rec (attach meta) t n e
  Eval meta e -> Eval (attach meta) e
  where
    attach (Meta toks) = (Meta $ toks <> [fromWs tokWithWs])

attachHiddenSemicolonToken :: Decl -> Decl
attachHiddenSemicolonToken = \case
  NonRec meta t n e -> NonRec (attach meta) t n e
  Rec meta t n e -> Rec (attach meta) t n e
  Eval meta e -> Eval (attach meta) e
  where
    attach m = m <> mkMeta [mkCsn $ mkHiddenClusterMeta Semicolon]

-- ----------------------------------------------------------------------------
-- Parser Utilities
-- ----------------------------------------------------------------------------

betweenP :: Parser open -> Parser close -> Parser b -> Parser (open, b, close)
betweenP open close p = do
  openWs <- open
  pWs <- p
  closeWs <- close
  pure (openWs, pWs, closeWs)

-- | @'sepByP' p sep@ parses /zero/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.
--
-- > commaSep p = p `sepBy` comma
sepByP :: Parser a -> Parser sep -> Parser ([a], [sep])
sepByP p sep = sepBy1P p sep <|> pure ([], [])
{-# INLINE sepByP #-}


-- | @'sepBy1P' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.
sepBy1P :: Parser a -> Parser sep -> Parser ([a], [sep])
sepBy1P p sep = do
  a <- p
  sepsAndA <- many ((,) <$> sep <*> p)
  let (seps, as) = unzip sepsAndA
  pure (a : as, seps)

-- | @'sepEndByP' p sep@ parses /zero/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.
sepEndByP :: Parser a -> Parser sep -> Parser ([a], [sep])
sepEndByP p sep = sepEndBy1P p sep <|> pure ([], [])
{-# INLINE sepEndByP #-}

-- | @'sepEndBy1P' p sep@ parses /one/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.
sepEndBy1P :: Parser a -> Parser sep -> Parser ([a], [sep])
sepEndBy1P p sep = do
  a <- p
  (as, seps) <-
    do
      s <- sep
      (as, seps) <- sepEndByP p sep
      pure (as, s : seps)
      <|> pure ([], [])
  pure (a : as, seps)
{-# INLINEABLE sepEndBy1P #-}

