module PostgresqlSyntax.Ast.ConfExpr where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.IndexParams
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
      [ WhereConfExpr <$> inParens parser <*> optional (Parser.space *> whereClause),
        ConstraintConfExpr <$> (keyword "on" *> Parser.space1 *> keyword "constraint" *> Parser.space1 *> Parser.endHead *> colId)
      ]
    where
      whereClause = keyword "where" *> Parser.space1 *> Parser.endHead *> parser

instance Qc.Arbitrary ConfExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ WhereConfExpr <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        ConstraintConfExpr <$> Qc.arbitrary
      ]
