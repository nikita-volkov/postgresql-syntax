module PostgresqlSyntax.Ast.FuncExpr where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.FuncApplication
import PostgresqlSyntax.Ast.FuncExprCommonSubexpr
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.OverClause
import PostgresqlSyntax.Ast.SortClause
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- func_expr:
--   | func_application within_group_clause filter_clause over_clause
--   | func_expr_common_subexpr
-- @
--
-- @within_group_clause@ and @filter_clause@ are bare aliases to
-- 'PostgresqlSyntax.Ast.SortClause' and 'PostgresqlSyntax.Ast.AExpr'
-- respectively.
data FuncExpr
  = ApplicationFuncExpr FuncApplication (Maybe SortClause) (Maybe AExpr) (Maybe OverClause)
  | SubexprFuncExpr FuncExprCommonSubexpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncExpr where
  toTextBuilder = \case
    ApplicationFuncExpr a b c d ->
      optLexemes
        [ Just (toTextBuilder a),
          fmap withinGroupClause b,
          fmap filterClause c,
          fmap toTextBuilder d
        ]
    SubexprFuncExpr a -> toTextBuilder a
    where
      withinGroupClause a = "WITHIN GROUP (" <> toTextBuilder a <> ")"
      filterClause a = "FILTER (WHERE " <> toTextBuilder a <> ")"
  parser =
    asum
      [ SubexprFuncExpr <$> parser,
        do
          a <- parser
          endHead
          b <- optional (space1 *> withinGroupClause)
          c <- optional (space1 *> filterClause)
          d <- optional (space1 *> parser)
          return (ApplicationFuncExpr a b c d)
      ]
    where
      withinGroupClause = do
        keyphrase "within group"
        endHead
        space
        inParens parser
      filterClause = do
        keyword "filter"
        endHead
        space
        inParens (keyword "where" *> space1 *> parser)

instance Arbitrary FuncExpr where
  arbitrary =
    oneof
      [ ApplicationFuncExpr
          <$> scale (`div` 4) arbitrary
          <*> scale (`div` 4) arbitrary
          <*> scale (`div` 4) arbitrary
          <*> scale (`div` 4) arbitrary,
        SubexprFuncExpr <$> scale (`div` 2) arbitrary
      ]
