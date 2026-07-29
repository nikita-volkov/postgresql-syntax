module PostgresqlSyntax.Ast.FrameClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.FrameClauseMode
import PostgresqlSyntax.Ast.FrameExtent
import PostgresqlSyntax.Ast.WindowExclusionClause
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
  toTextBuilder settings (FrameClause a b c) =
    TextBuilders.optLexemes
      [ Just (toTextBuilder settings a),
        Just (toTextBuilder settings b),
        fmap (toTextBuilder settings) c
      ]
  parser settings = do
    a <- parser settings <* Parsers.space1 <* Parser.endHead
    b <- parser settings
    c <- optional (Parsers.space1 *> parser settings)
    return (FrameClause a b c)

instance Qc.Arbitrary FrameClause where
  shrink = Qc.genericShrink
  arbitrary = FrameClause <$> arbitrary <*> arbitrary <*> arbitrary
