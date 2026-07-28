module PostgresqlSyntax.Ast.IndexElemDef where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.FuncExprWindowless
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder = \case
    IdIndexElemDef a -> toTextBuilder a
    FuncIndexElemDef a -> toTextBuilder a
    ExprIndexElemDef a -> TextBuilders.renderInParens (toTextBuilder a)
  parser =
    ExprIndexElemDef
      <$> Parsers.inParens parser
        <|> FuncIndexElemDef
      <$> parser
        <|> IdIndexElemDef
      <$> colId

instance Qc.Arbitrary IndexElemDef where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ IdIndexElemDef <$> Qc.arbitrary,
        FuncIndexElemDef <$> Qc.arbitrary,
        ExprIndexElemDef <$> Gens.downscale Qc.arbitrary
      ]
