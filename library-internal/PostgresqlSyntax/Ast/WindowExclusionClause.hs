module PostgresqlSyntax.Ast.WindowExclusionClause where

import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
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
  toTextBuilder _settings = \case
    CurrentRowWindowExclusionClause -> "EXCLUDE CURRENT ROW"
    GroupWindowExclusionClause -> "EXCLUDE GROUP"
    TiesWindowExclusionClause -> "EXCLUDE TIES"
    NoOthersWindowExclusionClause -> "EXCLUDE NO OTHERS"
  parser _settings =
    CurrentRowWindowExclusionClause
      <$ Parsers.keyphrase "exclude current row"
        <|> GroupWindowExclusionClause
      <$ Parsers.keyphrase "exclude group"
        <|> TiesWindowExclusionClause
      <$ Parsers.keyphrase "exclude ties"
        <|> NoOthersWindowExclusionClause
      <$ Parsers.keyphrase "exclude no others"

instance Qc.Arbitrary WindowExclusionClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.elements
      [ CurrentRowWindowExclusionClause,
        GroupWindowExclusionClause,
        TiesWindowExclusionClause,
        NoOthersWindowExclusionClause
      ]
