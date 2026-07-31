module PostgresqlSyntax.Ast.FrameClauseMode where

import PostgresqlSyntax.Algebra
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
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
data FrameClauseMode = RangeFrameClauseMode | RowsFrameClauseMode | GroupsFrameClauseMode
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FrameClauseMode where
  toTextBuilder _settings = \case
    RangeFrameClauseMode -> "RANGE"
    RowsFrameClauseMode -> "ROWS"
    GroupsFrameClauseMode -> "GROUPS"
  parser _settings =
    asum
      [ RangeFrameClauseMode <$ Parsers.keyword "range",
        RowsFrameClauseMode <$ Parsers.keyword "rows",
        GroupsFrameClauseMode <$ Parsers.keyword "groups"
      ]

instance Qc.Arbitrary FrameClauseMode where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [RangeFrameClauseMode, RowsFrameClauseMode, GroupsFrameClauseMode]
