{-# LANGUAGE LambdaCase #-}

module Simala.Expr.Metadata where

import Base.Text
import Data.List qualified as List
import Simala.Expr.Lexer
import qualified Data.Foldable as Foldable

data NodeVisibility
  = -- | A token cluster that is hidden because it was inserted by some tool.
    -- The user did not write it.
    Hidden
  | -- | A token written by the user.
    Visible
  deriving stock (Show, Ord, Eq, Enum, Bounded)

data ConcreteSyntaxNode = ConcreteSyntaxNode
  { tokens :: [PosToken]
  , tokenRange :: Maybe SrcRange
  , tokenVisibility :: NodeVisibility
  }
  deriving stock (Show, Ord, Eq)

data ClusterMeta = ClusterMeta
  { payload :: ConcreteSyntaxNode
  , trailing :: ConcreteSyntaxNode
  , label :: Text
  }
  deriving stock (Show, Ord, Eq)

data MetaElement
  = MetaHole
  | MetaCsn ClusterMeta
  deriving stock (Show, Ord, Eq)

mkHoleWithType :: a -> MetaElement
mkHoleWithType _ = MetaHole

mkHole :: MetaElement
mkHole = MetaHole

mkCsn :: ClusterMeta -> MetaElement
mkCsn = MetaCsn

data Meta = Meta
  { cstTokens :: [MetaElement]
  }
  deriving stock (Show, Ord, Eq)

instance Semigroup Meta where
  (Meta m1) <> (Meta m2) = Meta (m1 <> m2)

instance Monoid Meta where
  mempty = emptyMeta

csTokens :: ClusterMeta -> [PosToken]
csTokens cluster = cluster.payload.tokens <> cluster.trailing.tokens

mkMeta :: [MetaElement] -> Meta
mkMeta = Meta

emptyMeta :: Meta
emptyMeta = mkMeta []

isEmptyMeta :: Meta -> Bool
isEmptyMeta m = List.null m.cstTokens

mkConcreteSyntaxNode :: [PosToken] -> ConcreteSyntaxNode
mkConcreteSyntaxNode posTokens =
  ConcreteSyntaxNode
    { tokens = posTokens
    , tokenRange = Nothing -- TODO fendor: fix this
    , tokenVisibility =
        if Foldable.null posTokens
          then Hidden
          else Visible
    }

mkHiddenClusterMeta :: TokenType -> ClusterMeta
mkHiddenClusterMeta tt =
  ClusterMeta
    { payload = mkConcreteSyntaxNode []
    , trailing = mkConcreteSyntaxNode []
    , label = tokenTypeLabel tt
    }

data Lookup
  = Exact Text
  | After Text

queryLabel :: Lookup -> Text
queryLabel = \case
  Exact label -> label
  After label -> label

-- lookupTokens :: (HasCallStack) => Lookup -> Meta -> ConcreteSyntaxNode
-- lookupTokens query m =
--   let
--     tokenMeta = lookupTokensByLabel (queryLabel query) m
--   in
--     case query of
--       Exact{} -> tokenMeta.thisToken
--       After{} -> tokenMeta.afterThisToken

-- lookupTokensByLabel :: (HasCallStack) => Text -> Meta -> ClusterMeta
-- lookupTokensByLabel csnLabel m =
--   case List.find (\t -> t.label == csnLabel) m.cstTokens of
--     Nothing ->
--       error $
--         "lookupTokensByLabel: Concrete Syntax Node: \""
--           <> unpack csnLabel
--           <> "\" not found in: "
--           <> show (fmap (\t -> unpack t.label) m.cstTokens)
--     Just meta -> meta
