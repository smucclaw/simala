{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Simala.Expr.Lexer where

import Base
import qualified Base.Map as Map
import qualified Base.Text as Text

import Data.Char hiding (Space)
import Data.Proxy
import Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.State
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Lexer = Parsec Void Text

-- | Megaparsec stores offsets as integers.
type Offset = Int

-- | A raw token is a token with offset information attached by Megaparsec.
data RawToken =
  MkRawToken
    { start   :: !Offset
    , payload :: !TokenType
    , end     :: !Offset
    }
  deriving stock Show

-- | A pos token is a token with position information attached.
data PosToken =
  MkPosToken
    { range   :: !SrcRange
    , payload :: !TokenType
    }
  deriving stock (Eq, Ord, Show)

-- | A range of source positions. We (somewhat redundantly) store its length
-- as well.
data SrcRange =
  MkSrcRange
    { start   :: !SrcPos -- inclusive
    , end     :: !SrcPos -- inclusive
    , length  :: !Int
    }
  deriving stock (Eq, Ord, Show)

-- | A single source position. Line and column numbers are 1-based.
data SrcPos =
  MkSrcPos
    { filename :: !FilePath
    , line     :: !Int
    , column   :: !Int
    }
  deriving stock (Eq, Ord, Show)

-- | The type of token, plus information needed to reconstruct its contents.
data TokenType =
    Identifier    !Text
  | Quoted        !Text
  | TIntLit       !Int
  | TFracLit      !Double
  | TStringLit    !Text
  | Directive     !Text
    -- parentheses
  | POpen
  | PClose
  | COpen
  | CClose
  | SOpen
  | SClose
    -- punctuation
  | Comma
  | Semicolon
  | Dot
  | Implies
    -- symbolic operators
  | Times
  | Divides
  | Percent
  | Plus
  | TMinus
  | PlusPlus
  | Colon
  | GreaterEquals
  | LessEquals
  | GreaterThan
  | LessThan
  | Equals
  | EqualsEquals
  | NotEquals
  | ApproxEquals
  | TAnd
  | TOr
  | Quote
  | OtherSymbolic !Text
    -- keywords
  | KFalse
  | KTrue
  | KLet
  | KLetrec
  | KIn
  | KIf
  | KThen
  | KElse
  | KFun
  | KUndefined
  | KOpaque
  | KTransparent
  | KRec
    -- space
  | WhiteSpace   !Text
  | LineComment  !Text
  | BlockComment !Text
    -- other
  | EOF
  deriving stock (Eq, Generic, Ord, Show)

whitespace :: Lexer Text
whitespace =
  takeWhile1P (Just "whitespace") isSpace

lineComment :: Lexer Text
lineComment =
  (<>) <$> string "--" <*> takeWhileP (Just "character") (/= '\n')

blockComment :: Lexer Text
blockComment =
  (\ b (c, e) -> Text.concat (b : c ++ [e])) <$> string "{-" <*> manyTill_ inner (string "-}")
  where
    inner = blockComment <|> Text.singleton <$> anySingle

stringLiteral :: Lexer Text
stringLiteral =
  char '"' *> (Text.pack <$> manyTill Lexer.charLiteral (char '"'))

-- | A quoted identifier between backticks.
quoted :: Lexer Text
quoted =
  char '`' *> takeWhile1P (Just "printable char except backticks") (\ x -> isPrint x && not (x `elem` ("`" :: String))) <* char '`'

directive :: Lexer Text
directive =
  char '#' *> identifier

integerLiteral :: Lexer Int
integerLiteral =
      negate <$ string "-" <*> Lexer.decimal
  <|> Lexer.decimal

fractionalLiteral :: Lexer Double
fractionalLiteral =
      negate <$ string "-" <*> Lexer.float
  <|> Lexer.float

tokenPayload :: Lexer TokenType
tokenPayload =
      TFracLit     <$> try fractionalLiteral
  <|> TIntLit      <$> try integerLiteral
  <|> TStringLit   <$> stringLiteral
  <|> Quoted       <$> quoted
  <|> WhiteSpace   <$> whitespace
  <|> LineComment  <$> lineComment
  <|> BlockComment <$> blockComment
  <|> Directive    <$> directive
  <|> POpen        <$ char '('
  <|> PClose       <$ char ')'
  <|> COpen        <$ char '{'
  <|> CClose       <$ char '}'
  <|> SOpen        <$ char '['
  <|> SClose       <$ char ']'
  <|> Comma        <$ char ','
  <|> Semicolon    <$ char ';'
  <|> Dot          <$ char '.'
  <|> Quote        <$ char '\''
  <|> symbolic
  <|> identifierOrKeyword

symbolic :: Lexer TokenType
symbolic =
  do
    s <- symbolString
    case Map.lookup s symbols of
      Nothing -> pure (OtherSymbolic s)
      Just tt -> pure tt

symbolString :: Lexer Text
symbolString =
  takeWhile1P (Just "symbol char") (\ x -> x `elem` ("=<>+-*/:~&|%" :: [Char]))

identifierOrKeyword :: Lexer TokenType
identifierOrKeyword =
  do
    i <- identifier
    case Map.lookup i keywords of
      Nothing -> pure (Identifier i)
      Just tt -> pure tt

identifier :: Lexer Text
identifier =
  Text.cons
  <$> satisfy isAlpha
  <*> takeWhileP (Just "identifier char") (\ x -> isAlphaNum x || x == '_')

symbols :: Map Text TokenType
symbols =
  Map.fromList
    [ ("=>", Implies      )
    , ("*" , Times        )
    , ("/" , Divides      )
    , ("%" , Percent      )
    , ("+" , Plus         )
    , ("-" , TMinus       )
    , ("++", PlusPlus     )
    , (":" , Colon        )
    , (">=", GreaterEquals)
    , ("<=", LessEquals   )
    , (">" , GreaterThan  )
    , ("<" , LessThan     )
    , ("=" , Equals       )
    , ("==", EqualsEquals )
    , ("~=", ApproxEquals )
    , ("&&", TAnd         )
    , ("||", TOr          )
    ]

keywords :: Map Text TokenType
keywords =
  Map.fromList
    [ ("false"      , KFalse      )
    , ("true"       , KTrue       )
    , ("let"        , KLet        )
    , ("letrec"     , KLetrec     )
    , ("in"         , KIn         )
    , ("if"         , KIf         )
    , ("then"       , KThen       )
    , ("else"       , KElse       )
    , ("fun"        , KFun        )
    , ("undefined"  , KUndefined  )
    , ("opaque"     , KOpaque     )
    , ("transparent", KTransparent)
    , ("rec"        , KRec        )
    ]

rawTokens :: Lexer [RawToken]
rawTokens = many (MkRawToken <$> getOffset <*> tokenPayload <*> getOffset)

execLexer :: FilePath -> Text -> Either String [PosToken]
execLexer filepath txt =
  let
    r = runParser (rawTokens <* eof) filepath txt
  in
    case r of
      Right rtoks -> Right (mkPosTokens filepath txt rtoks)
      Left err    -> Left (errorBundlePretty err)

mkPosTokens :: FilePath -> Text -> [RawToken] -> [PosToken]
mkPosTokens filepath txt rtoks =
    evalState (traverse go rtoks) (initialPosState filepath txt)
  where
    go :: RawToken -> StateT (PosState Text) Identity PosToken
    go rtok = do
      pst <- get
      let
        pstStart = reachOffsetNoLine rtok.start pst
        pstEnd   = reachOffsetNoLine rtok.end pstStart
      put pstEnd
      pure
        (MkPosToken
          (MkSrcRange
            (convertPos (pstateSourcePos pstStart))
            (convertPos (pstateSourcePos pstEnd  ))
            (rtok.end - rtok.start)
          )
          rtok.payload
        )

-- | Convert from a Megaparsec source position to one of ours.
convertPos :: SourcePos -> SrcPos
convertPos (SourcePos fn l c) =
  MkSrcPos fn (unPos l) (unPos c)

unconvertPos :: SrcPos -> SourcePos
unconvertPos (MkSrcPos fn l c) =
  SourcePos fn (mkPos l) (mkPos c)

data TokenStream =
  MkTokenStream
    { input  :: String
    , tokens :: [PosToken]
    }
  deriving stock Show

proxyTokenStream :: Proxy TokenStream
proxyTokenStream = Proxy

instance Stream TokenStream where
  type Token TokenStream = PosToken
  type Tokens TokenStream = [PosToken]

  tokenToChunk :: Proxy TokenStream -> Token TokenStream -> Tokens TokenStream
  tokenToChunk Proxy x = [x]

  tokensToChunk :: Proxy TokenStream -> [Token TokenStream] -> Tokens TokenStream
  tokensToChunk Proxy xs = xs

  chunkToTokens :: Proxy TokenStream -> Tokens TokenStream -> [Token TokenStream]
  chunkToTokens Proxy xs = xs

  chunkLength :: Proxy TokenStream -> Tokens TokenStream -> Int
  chunkLength Proxy xs = length xs

  chunkEmpty :: Proxy TokenStream -> Tokens TokenStream -> Bool
  chunkEmpty Proxy = null

  take1_ :: TokenStream -> Maybe (Token TokenStream, TokenStream)
  take1_ (MkTokenStream _ [])         = Nothing
  take1_ (MkTokenStream txt (t : ts)) =
    Just (t, MkTokenStream (drop (tokensLength proxyTokenStream (t :| [])) txt) ts)

  takeN_ :: Int -> TokenStream -> Maybe (Tokens TokenStream, TokenStream)
  takeN_ n (MkTokenStream txt ts)
    | n <= 0 = Just ([], MkTokenStream txt ts)
    | null ts = Nothing
    | otherwise =
      let
        (ts1, ts2) = splitAt n ts
      in
        case nonEmpty ts1 of
          Nothing -> Just (ts1, MkTokenStream txt ts2)
          Just nex -> Just (ts1, MkTokenStream (drop (tokensLength proxyTokenStream nex) txt) ts2)

  takeWhile_ :: (Token TokenStream -> Bool) -> TokenStream -> (Tokens TokenStream, TokenStream)
  takeWhile_ p (MkTokenStream txt ts) =
    let
      (ts1, ts2) = span p ts
    in
      case nonEmpty ts1 of
        Nothing  -> (ts1, MkTokenStream txt ts2)
        Just nex -> (ts1, MkTokenStream (drop (tokensLength proxyTokenStream nex) txt) ts2)

instance VisualStream TokenStream where
  showTokens :: Proxy TokenStream -> NonEmpty (Token TokenStream) -> String
  showTokens Proxy xs =
    concat (toList (Text.unpack . displayPosToken <$> xs))

  tokensLength :: Proxy TokenStream -> NonEmpty (Token TokenStream) -> Int
  tokensLength Proxy xs = sum ((.range.length) <$> xs)

instance TraversableStream TokenStream where
  reachOffset :: Int -> PosState TokenStream -> (Maybe String, PosState TokenStream)
  reachOffset o pst =
    ( Just (prefix ++ restOfLine)
    , PosState
        { pstateInput      = MkTokenStream postTxt post
        , pstateOffset     = max pst.pstateOffset o
        , pstateSourcePos  = newSourcePos
        , pstateTabWidth   = pst.pstateTabWidth
        , pstateLinePrefix = prefix
        }
    )
    where
      prefix
        | sameLine  = pst.pstateLinePrefix ++ preLine
        | otherwise = preLine

      sameLine :: Bool
      sameLine = sourceLine newSourcePos == sourceLine pst.pstateSourcePos

      newSourcePos :: SourcePos
      newSourcePos =
        case post of
          [] -> case pst.pstateInput.tokens of
            [] -> pst.pstateSourcePos
            xs -> unconvertPos (last xs).range.end
          (x : _) -> unconvertPos x.range.start

      pre, post :: [PosToken]
      (pre, post) = splitAt (o - pst.pstateOffset) pst.pstateInput.tokens

      preTxt, postTxt :: String
      (preTxt, postTxt) = splitAt tokensConsumed pst.pstateInput.input

      preLine :: String
      preLine = reverse . takeWhile (/= '\n') . reverse $ preTxt

      tokensConsumed :: Int
      tokensConsumed =
        case nonEmpty pre of
          Nothing    -> 0
          Just nePre -> tokensLength proxyTokenStream nePre

      restOfLine :: String
      restOfLine = takeWhile (/= '\n') postTxt

displayPosToken :: PosToken -> Text
displayPosToken (MkPosToken _r tt) =
  case tt of
    Identifier t    -> t
    Quoted t        -> "`" <> t <> "`"
    TIntLit i       -> Text.pack (show i) -- it'd probably be better to store the text, so that we can reproduce it
    TFracLit d      -> Text.pack (show d) -- it'd probably be better to store the text, so that we can reproduce it
    TStringLit s    -> Text.pack (show s) -- ideally, this should be fine, because we use the Haskell escape sequences
    Directive s     -> "#" <> s
    POpen           -> "("
    PClose          -> ")"
    COpen           -> "{"
    CClose          -> "}"
    SOpen           -> "["
    SClose          -> "]"
    Comma           -> ","
    Semicolon       -> ";"
    Dot             -> "."
    Implies         -> "=>"
    Times           -> "*"
    Divides         -> "/"
    Percent         -> "%"
    Plus            -> "+"
    TMinus          -> "-"
    PlusPlus        -> "++"
    Colon           -> ":"
    GreaterEquals   -> ">="
    LessEquals      -> "<="
    GreaterThan     -> ">"
    LessThan        -> "<"
    Equals          -> "="
    EqualsEquals    -> "=="
    NotEquals       -> "/="
    ApproxEquals    -> "!="
    TAnd            -> "&&"
    TOr             -> "||"
    Quote           -> "'"
    OtherSymbolic t -> t
    KFalse          -> "false"
    KTrue           -> "true"
    KLet            -> "let"
    KLetrec         -> "letrec"
    KIn             -> "in"
    KIf             -> "if"
    KThen           -> "then"
    KElse           -> "else"
    KFun            -> "fun"
    KUndefined      -> "undefined"
    KOpaque         -> "opaque"
    KTransparent    -> "transparent"
    KRec            -> "rec"
    WhiteSpace t    -> t
    LineComment t   -> t
    BlockComment t  -> t
    EOF             -> ""

tokenTypeLabel :: TokenType -> Text
tokenTypeLabel =
  \case
    Identifier _    -> "identifier"
    Quoted _        -> "identifier"
    TIntLit _       -> "int"
    TFracLit _      -> "frac"
    TStringLit _    -> "string"
    Directive _     -> "directive"
    POpen           -> "("
    PClose          -> ")"
    COpen           -> "{"
    CClose          -> "}"
    SOpen           -> "["
    SClose          -> "]"
    Comma           -> ","
    Semicolon       -> ";"
    Dot             -> "."
    Implies         -> "=>"
    Times           -> "*"
    Divides         -> "/"
    Percent         -> "%"
    Plus            -> "+"
    TMinus          -> "-"
    PlusPlus        -> "++"
    Colon           -> ":"
    GreaterEquals   -> ">="
    LessEquals      -> "<="
    GreaterThan     -> ">"
    LessThan        -> "<"
    Equals          -> "="
    EqualsEquals    -> "=="
    NotEquals       -> "/="
    ApproxEquals    -> "!="
    TAnd            -> "&&"
    TOr             -> "||"
    Quote           -> "'"
    OtherSymbolic _ -> "symbol"
    KFalse          -> "false"
    KTrue           -> "true"
    KLet            -> "let"
    KLetrec         -> "letrec"
    KIn             -> "in"
    KIf             -> "if"
    KThen           -> "then"
    KElse           -> "else"
    KFun            -> "fun"
    KUndefined      -> "undefined"
    KOpaque         -> "visibility"
    KTransparent    -> "visibility"
    KRec            -> "rec"
    WhiteSpace _    -> "whitespace"
    LineComment _   -> "linecomment"
    BlockComment _  -> "blockcomment"
    EOF             -> "eof"

data TokenCategory
  = CIdentifier
  | CStringLit
  | CNumberLit
  | CSymbol
  | COperator
  | CKeyword
  | CComment
  | CWhitespace
  | CDirective
  | CEOF

posTokenCategory :: TokenType -> TokenCategory
posTokenCategory =
  \case
    Identifier _    -> CIdentifier
    Quoted _        -> CIdentifier
    TIntLit _       -> CNumberLit
    TFracLit _      -> CNumberLit
    TStringLit _    -> CStringLit
    Directive _     -> CDirective
    POpen           -> CSymbol
    PClose          -> CSymbol
    COpen           -> CSymbol
    CClose          -> CSymbol
    SOpen           -> CSymbol
    SClose          -> CSymbol
    Comma           -> CSymbol
    Semicolon       -> CSymbol
    Dot             -> CSymbol
    Implies         -> CSymbol
    Times           -> COperator
    Divides         -> COperator
    Percent         -> COperator
    Plus            -> COperator
    TMinus          -> COperator
    PlusPlus        -> COperator
    Colon           -> CSymbol
    GreaterEquals   -> COperator
    LessEquals      -> COperator
    GreaterThan     -> COperator
    LessThan        -> COperator
    Equals          -> CSymbol
    EqualsEquals    -> COperator
    NotEquals       -> COperator
    ApproxEquals    -> COperator
    TAnd            -> COperator
    TOr             -> COperator
    Quote           -> CSymbol
    OtherSymbolic _ -> CSymbol
    KFalse          -> CKeyword
    KTrue           -> CKeyword
    KLet            -> CKeyword
    KLetrec         -> CKeyword
    KIn             -> CKeyword
    KIf             -> CKeyword
    KThen           -> CKeyword
    KElse           -> CKeyword
    KFun            -> CKeyword
    KUndefined      -> CKeyword
    KOpaque         -> CKeyword
    KTransparent    -> CKeyword
    KRec            -> CKeyword
    WhiteSpace _    -> CWhitespace
    LineComment _   -> CComment
    BlockComment _  -> CComment
    EOF             -> CEOF
