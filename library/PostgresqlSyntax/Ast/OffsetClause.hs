module PostgresqlSyntax.Ast.OffsetClause where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SelectFetchFirstValue
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- offset_clause:
--   | OFFSET select_offset_value
--   | OFFSET select_fetch_first_value row_or_rows
-- select_offset_value:
--   | a_expr
-- row_or_rows:
--   | ROW
--   | ROWS
-- @
data OffsetClause
  = ExprOffsetClause AExpr
  | FetchFirstOffsetClause SelectFetchFirstValue Bool
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OffsetClause where
  toTextBuilder = \case
    ExprOffsetClause a -> "OFFSET " <> toTextBuilder a
    FetchFirstOffsetClause a b -> "OFFSET " <> toTextBuilder a <> " " <> rowOrRows b
    where
      rowOrRows = bool "ROW" "ROWS"
  parser = do
    keyword "offset"
    endHead
    space1
    asum
      [ FetchFirstOffsetClause <$> wrapToHead parser <*> (space1 *> rowOrRows),
        ExprOffsetClause <$> parser
      ]
    where
      rowOrRows =
        True <$ keyword "rows"
          <|> False <$ keyword "row"

instance Arbitrary OffsetClause where
  arbitrary =
    oneof
      [ ExprOffsetClause <$> scale (`div` 2) arbitrary,
        FetchFirstOffsetClause <$> scale (`div` 2) arbitrary <*> arbitrary
      ]
