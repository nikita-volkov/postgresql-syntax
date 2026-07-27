module PostgresqlSyntax.Ast.FrameClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.FrameClauseMode
import PostgresqlSyntax.Ast.FrameExtent
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.WindowExclusionClause
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
    a <- parser <* Parser.space1 <* Parser.endHead
    b <- parser
    c <- optional (Parser.space1 *> parser)
    return (FrameClause a b c)

instance Qc.Arbitrary FrameClause where
  arbitrary = FrameClause <$> arbitrary <*> Qc.scale (`div` 2) arbitrary <*> arbitrary
