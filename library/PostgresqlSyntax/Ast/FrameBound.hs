module PostgresqlSyntax.Ast.FrameBound where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
      <$ keyphrase "unbounded preceding"
        <|> UnboundedFollowingFrameBound
      <$ keyphrase "unbounded following"
        <|> CurrentRowFrameBound
      <$ keyphrase "current row"
        <|> do
          a <- parser
          Parser.space1
          PrecedingFrameBound a <$ keyword "preceding" <|> FollowingFrameBound a <$ keyword "following"

instance Qc.Arbitrary FrameBound where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ pure UnboundedPrecedingFrameBound,
        pure UnboundedFollowingFrameBound,
        pure CurrentRowFrameBound,
        PrecedingFrameBound <$> Qc.scale (`div` 2) Qc.arbitrary,
        FollowingFrameBound <$> Qc.scale (`div` 2) Qc.arbitrary
      ]
