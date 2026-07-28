module PostgresqlSyntax.Ast.FrameBound where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- frame_bound:
--   |  UNBOUNDED PRECEDING
--   |  UNBOUNDED FOLLOWING
--   |  CURRENT_P ROW
--   |  a_expr PRECEDING
--   |  a_expr FOLLOWING
-- @
data FrameBound
  = UnboundedPrecedingFrameBound
  | UnboundedFollowingFrameBound
  | CurrentRowFrameBound
  | PrecedingFrameBound AExpr
  | FollowingFrameBound AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FrameBound where
  toTextBuilder = \case
    UnboundedPrecedingFrameBound -> "UNBOUNDED PRECEDING"
    UnboundedFollowingFrameBound -> "UNBOUNDED FOLLOWING"
    CurrentRowFrameBound -> "CURRENT ROW"
    PrecedingFrameBound a -> toTextBuilder a <> " PRECEDING"
    FollowingFrameBound a -> toTextBuilder a <> " FOLLOWING"
  parser =
    UnboundedPrecedingFrameBound
      <$ Parsers.keyphrase "unbounded preceding"
        <|> UnboundedFollowingFrameBound
      <$ Parsers.keyphrase "unbounded following"
        <|> CurrentRowFrameBound
      <$ Parsers.keyphrase "current row"
        <|> do
          a <- parser
          Parsers.space1
          PrecedingFrameBound a <$ Parsers.keyword "preceding" <|> FollowingFrameBound a <$ Parsers.keyword "following"

instance Qc.Arbitrary FrameBound where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ pure UnboundedPrecedingFrameBound,
        pure UnboundedFollowingFrameBound,
        pure CurrentRowFrameBound,
        PrecedingFrameBound <$> Gens.downscale Qc.arbitrary,
        FollowingFrameBound <$> Gens.downscale Qc.arbitrary
      ]
