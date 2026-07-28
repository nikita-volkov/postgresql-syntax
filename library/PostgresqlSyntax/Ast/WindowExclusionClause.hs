module PostgresqlSyntax.Ast.WindowExclusionClause where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- opt_window_exclusion_clause:
--   |  EXCLUDE CURRENT_P ROW
--   |  EXCLUDE GROUP_P
--   |  EXCLUDE TIES
--   |  EXCLUDE NO OTHERS
--   |  EMPTY
-- @
data WindowExclusionClause
  = CurrentRowWindowExclusionClause
  | GroupWindowExclusionClause
  | TiesWindowExclusionClause
  | NoOthersWindowExclusionClause
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst WindowExclusionClause where
  toTextBuilder = \case
    CurrentRowWindowExclusionClause -> "EXCLUDE CURRENT ROW"
    GroupWindowExclusionClause -> "EXCLUDE GROUP"
    TiesWindowExclusionClause -> "EXCLUDE TIES"
    NoOthersWindowExclusionClause -> "EXCLUDE NO OTHERS"
  parser =
    CurrentRowWindowExclusionClause
      <$ keyphrase "exclude current row"
        <|> GroupWindowExclusionClause
      <$ keyphrase "exclude group"
        <|> TiesWindowExclusionClause
      <$ keyphrase "exclude ties"
        <|> NoOthersWindowExclusionClause
      <$ keyphrase "exclude no others"

instance Qc.Arbitrary WindowExclusionClause where
  arbitrary =
    Qc.elements
      [ CurrentRowWindowExclusionClause,
        GroupWindowExclusionClause,
        TiesWindowExclusionClause,
        NoOthersWindowExclusionClause
      ]
