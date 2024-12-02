{-# LANGUAGE LambdaCase #-}

module Simala.Expr.Metadata where

import Base.Text
import Data.List qualified as List
import GHC.Stack
import Simala.Expr.Lexer
import qualified Data.Foldable as Foldable

data TokenVisibility
  = -- | A token that is hidden because it was inserted by some tool.
    -- The user did not write it.
    Hidden
  | -- | A token written by the user.
    Visible
  deriving stock (Show, Ord, Eq, Enum, Bounded)

data TokenCluster = TokenCluster
  { tokens :: [PosToken]
  , tokenRange :: Maybe SrcRange
  , tokenVisibility :: TokenVisibility
  }
  deriving stock (Show, Ord, Eq)

data ClusterMeta = ClusterMeta
  { thisToken :: TokenCluster
  , afterThisToken :: TokenCluster
  , label :: Text
  }
  deriving stock (Show, Ord, Eq)

data Meta = Meta
  { cstTokens :: [ClusterMeta]
  }
  deriving stock (Show, Ord, Eq)

instance Semigroup Meta where
  (Meta m1) <> (Meta m2) = Meta (m1 <> m2)

instance Monoid Meta where
  mempty = emptyMeta


tokensAt :: HasCallStack => Meta -> Int -> [PosToken]
tokensAt m n =
  let
    cluster = m.cstTokens !! n
  in
    csTokens cluster

csTokens :: ClusterMeta -> [PosToken]
csTokens cluster = cluster.thisToken.tokens <> cluster.afterThisToken.tokens

mkMeta :: [ClusterMeta] -> Meta
mkMeta = Meta

emptyMeta :: Meta
emptyMeta = mkMeta []

isEmptyMeta :: Meta -> Bool
isEmptyMeta m = List.null m.cstTokens

sizeOfMeta :: Meta -> Int
sizeOfMeta m = List.length m.cstTokens

mkTokenCluster :: [PosToken] -> TokenCluster
mkTokenCluster posTokens =
  TokenCluster
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
    { thisToken = mkTokenCluster []
    , afterThisToken = mkTokenCluster []
    , label = tokenTypeLabel tt
    }

data Lookup
  = Exact Text
  | After Text

queryLabel :: Lookup -> Text
queryLabel = \case
  Exact label -> label
  After label -> label

lookupTokens :: (HasCallStack) => Lookup -> Meta -> TokenCluster
lookupTokens query m =
  let
    tokenMeta = lookupTokensByLabel (queryLabel query) m
  in
    case query of
      Exact{} -> tokenMeta.thisToken
      After{} -> tokenMeta.afterThisToken

lookupTokensByLabel :: (HasCallStack) => Text -> Meta -> ClusterMeta
lookupTokensByLabel csnLabel m =
  case List.find (\t -> t.label == csnLabel) m.cstTokens of
    Nothing ->
      error $
        "lookupTokensByLabel: Concrete Syntax Node: \""
          <> unpack csnLabel
          <> "\" not found in: "
          <> show (fmap (\t -> unpack t.label) m.cstTokens)
    Just meta -> meta
