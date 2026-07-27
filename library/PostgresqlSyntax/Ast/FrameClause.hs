module PostgresqlSyntax.Ast.FrameClause where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.FrameClauseMode
import PostgresqlSyntax.Ast.FrameExtent
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.WindowExclusionClause
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- opt_frame_clause:
--   |  RANGE frame_extent opt_window_exclusion_clause
--   |  ROWS frame_extent opt_window_exclusion_clause
--   |  GROUPS frame_extent opt_window_exclusion_clause
--   |  EMPTY
-- @
data FrameClause = FrameClause FrameClauseMode FrameExtent (Maybe WindowExclusionClause)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FrameClause where
  toTextBuilder (FrameClause a b c) =
    optLexemes
      [ Just (toTextBuilder a),
        Just (toTextBuilder b),
        fmap toTextBuilder c
      ]
  parser = do
    a <- parser <* space1 <* endHead
    b <- parser
    c <- optional (space1 *> parser)
    return (FrameClause a b c)

instance Arbitrary FrameClause where
  arbitrary = FrameClause <$> arbitrary <*> scale (`div` 2) arbitrary <*> arbitrary
