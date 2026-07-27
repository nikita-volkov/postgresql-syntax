module PostgresqlSyntax.Ast.OnConflictDo where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SetClauseList
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- opt_on_conflict:
--   | ON CONFLICT opt_conf_expr DO UPDATE SET set_clause_list where_clause
--   | ON CONFLICT opt_conf_expr DO NOTHING
--   | EMPTY
-- @
--
-- @where_clause@ is a bare alias to 'PostgresqlSyntax.Ast.AExpr'.
data OnConflictDo
  = UpdateOnConflictDo SetClauseList (Maybe AExpr)
  | NothingOnConflictDo
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OnConflictDo where
  toTextBuilder = \case
    UpdateOnConflictDo a b -> "UPDATE SET " <> toTextBuilder a <> suffixMaybe whereClause b
    NothingOnConflictDo -> "NOTHING"
    where
      whereClause a = "WHERE " <> toTextBuilder a
  parser =
    asum
      [ NothingOnConflictDo <$ keyword "nothing",
        do
          keyword "update"
          space1
          endHead
          keyword "set"
          space1
          a <- parser
          b <- optional (space1 *> whereClause)
          return (UpdateOnConflictDo a b)
      ]
    where
      whereClause = keyword "where" *> space1 *> endHead *> parser

instance Arbitrary OnConflictDo where
  arbitrary =
    oneof
      [ UpdateOnConflictDo <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
        pure NothingOnConflictDo
      ]
