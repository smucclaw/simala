{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Simala.Expr.Lexer
import Simala.Expr.Metadata
import Simala.Expr.Parser (parseDecls)
import Simala.Expr.Type

import Control.Applicative (Alternative (..))
import Control.Lens hiding (Iso)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (Reader, ask, runReader, withReader)
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import GHC.Stack (HasCallStack)
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
import Language.LSP.VFS (VirtualFile (..))

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ -- We need these notifications handlers to declare that we handle these requests
      notificationHandler SMethod_Initialized mempty
    , -- Handling of the virtual file system
      notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
        let
          doc = msg ^. J.params . J.textDocument . J.uri
        sendDiagnostics $ LSP.toNormalizedUri doc
    , -- Subscribe to notification changes
      notificationHandler SMethod_WorkspaceDidChangeConfiguration mempty
    , requestHandler SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
        let
          TRequestMessage _ _ _ (SemanticTokensParams _ _ doc) = req
          uri = doc ^. J.uri

        mVirtFile <- getVirtualFile $ toNormalizedUri uri
        case mVirtFile of
          Nothing -> do
            responder $ Right $ InR Null
          Just (VirtualFile _ _ rope) -> do
            let
              contents = Rope.toText rope

            case parseSimalaWithDiagnostics uri contents of
              Left _diags -> responder $ Right $ InR Null
              Right ds -> do
                let
                  semanticTokens = relativizeTokens $ fmap toSemanticTokenAbsolute $ runReader (declsToSemanticTokens ds) defaultInfo
                case encodeTokens defaultSemanticTokensLegend semanticTokens of
                  Left _err -> do
                    responder $ Right $ InR Null
                  Right semanticTokensData -> do
                    responder $
                      Right $
                        InL $
                          SemanticTokens
                            { _resultId = Nothing
                            , _data_ = semanticTokensData
                            }
    ]

-- These settings are important!
syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just True
    , LSP._change = Just LSP.TextDocumentSyncKind_Incremental
    , LSP._willSave = Just False
    , LSP._willSaveWaitUntil = Just False
    , LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions
    , optExecuteCommandCommands = Just []
    }

main :: IO Int
main =
  runServer $
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = "simala"
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = \_caps -> handlers
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = lspOptions
      }

-- ----------------------------------------------------------------------------
-- LSP Diagnostics
-- ----------------------------------------------------------------------------

sendDiagnostics :: NormalizedUri -> LspM () ()
sendDiagnostics fileUri = do
  mfile <- getVirtualFile fileUri
  case mfile of
    Nothing -> pure ()
    Just (VirtualFile version _ rope) -> do
      let
        contents = Rope.toText rope

      diags <- case parseSimalaWithDiagnostics (fromNormalizedUri fileUri) contents of
        Left diags -> pure diags
        Right _ds -> pure []
      publishDiagnostics 100 fileUri (Just version) (partitionBySource diags)

-- ----------------------------------------------------------------------------
-- Simala Parser
-- ----------------------------------------------------------------------------

parseSimalaWithDiagnostics :: Uri -> Text -> Either [Diagnostic] [Decl]
parseSimalaWithDiagnostics uri content = case parseDecls fp content of
  Left err ->
    Left
      [ Diagnostic
          (LSP.Range (LSP.Position 0 0) (LSP.Position 1 0))
          (Just LSP.DiagnosticSeverity_Error) -- severity
          Nothing -- code
          Nothing
          (Just "parser") -- source
          ("Failed to parse program: " <> Text.pack err)
          Nothing -- tags
          (Just [])
          Nothing
      ]
  Right ds ->
    pure ds
 where
  fp = Maybe.fromMaybe "in-memory" $ uriToFilePath uri

-- ----------------------------------------------------------------------------
-- LSP Helpers
-- ----------------------------------------------------------------------------

data SemanticToken = SemanticToken
  { start :: Position
  , length :: UInt
  , category :: SemanticTokenTypes
  , modifiers :: [SemanticTokenModifiers]
  }
  deriving stock (Show, Eq, Ord)

toSemanticTokenAbsolute :: SemanticToken -> SemanticTokenAbsolute
toSemanticTokenAbsolute s =
  SemanticTokenAbsolute
    { _line = s.start ^. J.line
    , _startChar = s.start ^. J.character
    , _length = s.length
    , _tokenType = s.category
    , _tokenModifiers = s.modifiers
    }

standardTokenType :: TokenCategory -> Maybe SemanticTokenTypes
standardTokenType = \case
  CIdentifier -> Just SemanticTokenTypes_Variable
  CStringLit -> Just SemanticTokenTypes_String
  CNumberLit -> Just SemanticTokenTypes_Number
  CSymbol -> Just SemanticTokenTypes_Operator
  COperator -> Just SemanticTokenTypes_Operator
  CKeyword -> Just SemanticTokenTypes_Keyword
  CComment -> Just SemanticTokenTypes_Comment
  CWhitespace -> Nothing
  CDirective -> Just SemanticTokenTypes_Macro
  CEOF -> Nothing

simpleTokenType :: PosToken -> Maybe SemanticTokenTypes
simpleTokenType t = standardTokenType (posTokenCategory t.payload)

type HoleFit = [SemanticToken]

data SemanticTokenCtx p = Info
  { toSemanticToken :: p -> Maybe SemanticTokenTypes
  , getModifiers :: p -> Maybe [SemanticTokenModifiers]
  }

defaultInfo :: SemanticTokenCtx PosToken
defaultInfo =
  Info
    { toSemanticToken = simpleTokenType
    , getModifiers = \_ -> pure []
    }

parameterType :: TokenCategory -> Maybe SemanticTokenTypes
parameterType = \case
  CIdentifier -> Just SemanticTokenTypes_Parameter
  _ -> Nothing

enumType :: TokenCategory -> Maybe SemanticTokenTypes
enumType = \case
  CIdentifier -> Just SemanticTokenTypes_Enum
  _ -> Nothing

defVar :: TokenCategory -> Maybe [SemanticTokenModifiers]
defVar = \case
  CIdentifier -> Just [SemanticTokenModifiers_Declaration, SemanticTokenModifiers_Definition]
  _ -> Nothing

withModifier :: (TokenCategory -> Maybe [SemanticTokenModifiers]) -> Reader (SemanticTokenCtx PosToken) a -> Reader (SemanticTokenCtx PosToken) a
withModifier f = withReader (\i -> i{getModifiers = \t -> f (posTokenCategory t.payload) <|> i.getModifiers t})

withTokenType :: (TokenCategory -> Maybe SemanticTokenTypes) -> Reader (SemanticTokenCtx PosToken) a -> Reader (SemanticTokenCtx PosToken) a
withTokenType f = withReader (\i -> i{toSemanticToken = \t -> f (posTokenCategory t.payload) <|> i.toSemanticToken t})

-- ----------------------------------------------------------------------------
-- Simala AST to Semantic Tokens
-- ----------------------------------------------------------------------------

declsToSemanticTokens :: [Decl] -> Reader (SemanticTokenCtx PosToken) [SemanticToken]
declsToSemanticTokens ds = do
  dsToks <- traverse declSemanticTokens ds
  pure $ concat dsToks

declToSemanticTokens :: Decl -> Reader (SemanticTokenCtx PosToken) [SemanticToken]
declToSemanticTokens = declSemanticTokens

exprSemanticTokens :: Expr -> Reader (SemanticTokenCtx PosToken) [SemanticToken]
exprSemanticTokens = \case
  Builtin m _ es -> traverseCsnWithHoles m $ fmap exprSemanticTokens es
  Var m var -> traverseCsnWithHoles m [varSemanticTokens var]
  Atom m var -> traverseCsnWithHoles m [withTokenType enumType (varSemanticTokens var)]
  Lit m _l -> traverseCsnWithHoles m []
  App m e es -> traverseCsnWithHoles m $ [exprSemanticTokens e] <> fmap exprSemanticTokens es
  Record m rows -> traverseCsnWithHoles m [rowsSemanticTokens rows]
  Project m e var -> traverseCsnWithHoles m [exprSemanticTokens e, varSemanticTokens var]
  Fun m _t args e -> traverseCsnWithHoles m $ concat [fmap (withTokenType parameterType . varSemanticTokens) args, [exprSemanticTokens e]]
  Let m d e
    | (ds, inExpr) <- collectLetDecls e ->
        traverseCsnWithHoles m ([declsToSemanticTokens (d : ds)] <> [exprSemanticTokens inExpr])
  Undefined m -> traverseCsnWithHoles m []
  Parens m e -> traverseCsnWithHoles m [exprSemanticTokens e]

traverseCsnWithHoles :: (HasCallStack) => Meta -> [Reader (SemanticTokenCtx PosToken) HoleFit] -> Reader (SemanticTokenCtx PosToken) [SemanticToken]
traverseCsnWithHoles (Meta []) _ = pure []
traverseCsnWithHoles (Meta (MetaHole : cs)) holeFits = case holeFits of
  [] -> error $ "applyTokensWithHoles: HoleFit requested, but not enough Fits given."
  (x : xs) -> do
    toks <- x
    restOfTokens <- traverseCsnWithHoles (Meta cs) xs
    pure $ toks <> restOfTokens
traverseCsnWithHoles (Meta (MetaCsn m : cs)) xs = do
  ctx <- ask
  let
    transformSyntaxNode token = pack token <$> ctx.toSemanticToken token <*> ctx.getModifiers token
    thisSyntaxNode = Maybe.mapMaybe transformSyntaxNode (csTokens m)

  restOfTokens <- traverseCsnWithHoles (Meta cs) xs
  pure $ thisSyntaxNode <> restOfTokens
 where
  pack :: PosToken -> SemanticTokenTypes -> [SemanticTokenModifiers] -> SemanticToken
  pack token category modifiers =
    SemanticToken
      { start = srcPosToPosition token.range.start
      , length = fromIntegral token.range.length
      , category = category
      , modifiers = modifiers
      }

srcPosToPosition :: SrcPos -> Position
srcPosToPosition s =
  Position
    { _character = fromIntegral s.column - 1
    , _line = fromIntegral s.line - 1
    }

collectLetDecls :: Expr -> ([Decl], Expr)
collectLetDecls = go []
 where
  go ds (Let m d e)
    | isEmptyMeta m = go (ds <> [d]) e
  go ds e = (ds, e)

rowsSemanticTokens :: Rows Expr -> Reader (SemanticTokenCtx PosToken) [SemanticToken]
rowsSemanticTokens (Rows m rows) = traverseCsnWithHoles m $ fmap rowSemanticTokens rows

rowSemanticTokens :: Row Expr -> Reader (SemanticTokenCtx PosToken) [SemanticToken]
rowSemanticTokens (Row m var e) = traverseCsnWithHoles m [varSemanticTokens var, exprSemanticTokens e]

varSemanticTokens :: Variable -> Reader (SemanticTokenCtx PosToken) [SemanticToken]
varSemanticTokens (Variable m _name) = traverseCsnWithHoles m []

declSemanticTokens :: Decl -> Reader (SemanticTokenCtx PosToken) [SemanticToken]
declSemanticTokens = \case
  NonRec m _t name e -> traverseCsnWithHoles m [withModifier defVar $ varSemanticTokens name, exprSemanticTokens e]
  Rec m _t name e -> traverseCsnWithHoles m [withModifier defVar $ varSemanticTokens name, exprSemanticTokens e]
  Eval m e -> traverseCsnWithHoles m [exprSemanticTokens e]
