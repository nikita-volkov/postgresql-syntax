module PostgresqlSyntax.Ast.FrameBound where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

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
        space1
        PrecedingFrameBound a <$ keyword "preceding" <|> FollowingFrameBound a <$ keyword "following"

instance Arbitrary FrameBound where
  arbitrary =
    oneof
      [ pure UnboundedPrecedingFrameBound,
        pure UnboundedFollowingFrameBound,
        pure CurrentRowFrameBound,
        PrecedingFrameBound <$> scale (`div` 2) arbitrary,
        FollowingFrameBound <$> scale (`div` 2) arbitrary
      ]
