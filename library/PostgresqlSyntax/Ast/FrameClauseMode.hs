module PostgresqlSyntax.Ast.FrameClauseMode where

import PostgresqlSyntax.Ast.Internal
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
data FrameClauseMode = RangeFrameClauseMode | RowsFrameClauseMode | GroupsFrameClauseMode
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FrameClauseMode where
  toTextBuilder = \case
    RangeFrameClauseMode -> "RANGE"
    RowsFrameClauseMode -> "ROWS"
    GroupsFrameClauseMode -> "GROUPS"
  parser =
    asum
      [ RangeFrameClauseMode <$ keyword "range",
        RowsFrameClauseMode <$ keyword "rows",
        GroupsFrameClauseMode <$ keyword "groups"
      ]

instance Qc.Arbitrary FrameClauseMode where
  arbitrary = Qc.elements [RangeFrameClauseMode, RowsFrameClauseMode, GroupsFrameClauseMode]
