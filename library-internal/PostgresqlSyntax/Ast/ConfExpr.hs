module PostgresqlSyntax.Ast.ConfExpr where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.IndexParams
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
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
-- @where_clause@ is inlined here as a bare 'PostgresqlSyntax.Ast.AExpr'
-- rather than going through 'PostgresqlSyntax.Ast.WhereClause'.
-- @name@ is a bare alias to 'PostgresqlSyntax.Ast.Ident'.
data ConfExpr
  = WhereConfExpr IndexParams (Maybe AExpr)
  | ConstraintConfExpr Ident
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ConfExpr where
  toTextBuilder settings = \case
    WhereConfExpr a b -> TextBuilders.renderInParens (toTextBuilder settings a) <> TextBuilders.suffixMaybe whereClause b
    ConstraintConfExpr a -> "ON CONSTRAINT " <> toTextBuilder settings a
    where
      whereClause a = "WHERE " <> toTextBuilder settings a
  parser settings =
    asum
      [ WhereConfExpr <$> Parsers.inParens (parser settings) <*> optional (Parsers.space *> whereClause),
        ConstraintConfExpr <$> (Parsers.keyword "on" *> Parsers.space1 *> Parsers.keyword "constraint" *> Parsers.space1 *> Parser.endHead *> colId settings)
      ]
    where
      whereClause = Parsers.keyword "where" *> Parsers.space1 *> Parser.endHead *> parser settings

instance Qc.Arbitrary ConfExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ WhereConfExpr <$> Qc.arbitrary <*> Gens.terminatingMaybe (Gens.downscale Qc.arbitrary),
        ConstraintConfExpr <$> Qc.arbitrary
      ]
