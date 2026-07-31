module PostgresqlSyntax.Ast.IndexElemDef where

import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.FuncExprWindowless
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
--   | ColId opt_collate opt_class opt_asc_desc opt_nulls_order
--   | func_expr_windowless opt_collate opt_class opt_asc_desc opt_nulls_order
--   | '(' a_expr ')' opt_collate opt_class opt_asc_desc opt_nulls_order
-- @
data IndexElemDef
  = IdIndexElemDef Ident
  | FuncIndexElemDef FuncExprWindowless
  | ExprIndexElemDef AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst IndexElemDef where
  toTextBuilder settings = \case
    IdIndexElemDef a -> toTextBuilder settings a
    FuncIndexElemDef a -> toTextBuilder settings a
    ExprIndexElemDef a -> TextBuilders.renderInParens (toTextBuilder settings a)
  parser settings =
    ExprIndexElemDef
      <$> Parsers.inParens (parser settings)
        <|> FuncIndexElemDef
      <$> parser settings
        <|> IdIndexElemDef
      <$> colId settings

instance Qc.Arbitrary IndexElemDef where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ IdIndexElemDef <$> Qc.arbitrary,
        FuncIndexElemDef <$> Qc.arbitrary,
        ExprIndexElemDef <$> Gens.downscale Qc.arbitrary
      ]
