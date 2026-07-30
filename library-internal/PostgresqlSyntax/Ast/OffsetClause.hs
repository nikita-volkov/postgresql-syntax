module PostgresqlSyntax.Ast.OffsetClause where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.SelectFetchFirstValue
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
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
  toTextBuilder settings = \case
    ExprOffsetClause a -> "OFFSET " <> toTextBuilder settings a
    FetchFirstOffsetClause a b -> "OFFSET " <> toTextBuilder settings a <> " " <> rowOrRows b
    where
      rowOrRows = bool "ROW" "ROWS"
  parser settings = do
    Parsers.keyword "offset"
    Parser.endHead
    Parsers.space1
    asum
      [ FetchFirstOffsetClause <$> Parser.wrapToHead (parser settings) <*> (Parsers.space1 *> rowOrRows),
        ExprOffsetClause <$> parser settings
      ]
    where
      rowOrRows =
        True
          <$ Parsers.keyword "rows"
            <|> False
          <$ Parsers.keyword "row"

instance Qc.Arbitrary OffsetClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprOffsetClause <$> Gens.downscale Qc.arbitrary,
        FetchFirstOffsetClause <$> Qc.arbitrary <*> Qc.arbitrary
      ]
