module PostgresqlSyntax.Ast.IndexElemDef where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.FuncExprWindowless
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
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
    ExprIndexElemDef a -> renderInParens (toTextBuilder a)
  parser =
    ExprIndexElemDef
      <$> inParens parser
        <|> FuncIndexElemDef
      <$> parser
        <|> IdIndexElemDef
      <$> colId

instance Qc.Arbitrary IndexElemDef where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ IdIndexElemDef <$> Qc.arbitrary,
        FuncIndexElemDef <$> Qc.scale (`div` 2) Qc.arbitrary,
        ExprIndexElemDef <$> Qc.scale (`div` 2) Qc.arbitrary
      ]
