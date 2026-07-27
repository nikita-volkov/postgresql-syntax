module PostgresqlSyntax.Ast.ConfExpr where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.IndexParams
import PostgresqlSyntax.Ast.Internal
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- opt_conf_expr:
--   | '(' index_params ')' where_clause
--   | ON CONSTRAINT name
--   | EMPTY
-- @
--
-- @where_clause@\/@name@ are bare aliases to 'PostgresqlSyntax.Ast.AExpr'\/
-- 'PostgresqlSyntax.Ast.Ident'.
data ConfExpr
  = WhereConfExpr IndexParams (Maybe AExpr)
  | ConstraintConfExpr Ident
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ConfExpr where
  toTextBuilder = \case
    WhereConfExpr a b -> renderInParens (toTextBuilder a) <> suffixMaybe whereClause b
    ConstraintConfExpr a -> "ON CONSTRAINT " <> toTextBuilder a
    where
      whereClause a = "WHERE " <> toTextBuilder a
  parser =
    asum
      [ WhereConfExpr <$> inParens parser <*> optional (space *> whereClause),
        ConstraintConfExpr <$> (keyword "on" *> space1 *> keyword "constraint" *> space1 *> endHead *> colId)
      ]
    where
      whereClause = keyword "where" *> space1 *> endHead *> parser

instance Arbitrary ConfExpr where
  arbitrary =
    oneof
      [ WhereConfExpr <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
        ConstraintConfExpr <$> arbitrary
      ]
