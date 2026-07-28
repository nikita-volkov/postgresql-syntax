module PostgresqlSyntax.Ast.OffsetClause where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SelectFetchFirstValue
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
    Parser.endHead
    Parser.space1
    asum
      [ FetchFirstOffsetClause <$> Parser.wrapToHead parser <*> (Parser.space1 *> rowOrRows),
        ExprOffsetClause <$> parser
      ]
    where
      rowOrRows =
        True
          <$ keyword "rows"
            <|> False
          <$ keyword "row"

instance Qc.Arbitrary OffsetClause where
  arbitrary =
    Qc.oneof
      [ ExprOffsetClause <$> Qc.scale (`div` 2) Qc.arbitrary,
        FetchFirstOffsetClause <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary
      ]
