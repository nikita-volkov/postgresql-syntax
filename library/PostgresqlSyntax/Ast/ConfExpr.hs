module PostgresqlSyntax.Ast.ConfExpr where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.IndexParams
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
    WhereConfExpr a b -> TextBuilders.renderInParens (toTextBuilder a) <> TextBuilders.suffixMaybe whereClause b
    ConstraintConfExpr a -> "ON CONSTRAINT " <> toTextBuilder a
    where
      whereClause a = "WHERE " <> toTextBuilder a
  parser =
    asum
      [ WhereConfExpr <$> Parsers.inParens parser <*> optional (Parsers.space *> whereClause),
        ConstraintConfExpr <$> (Parsers.keyword "on" *> Parsers.space1 *> Parsers.keyword "constraint" *> Parsers.space1 *> Parser.endHead *> colId)
      ]
    where
      whereClause = Parsers.keyword "where" *> Parsers.space1 *> Parser.endHead *> parser

instance Qc.Arbitrary ConfExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ WhereConfExpr <$> Qc.arbitrary <*> Qc.terminatingMaybe (Qc.downscale Qc.arbitrary),
        ConstraintConfExpr <$> Qc.arbitrary
      ]
