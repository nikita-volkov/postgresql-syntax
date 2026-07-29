module PostgresqlSyntax.Ast.FuncExpr where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.FuncApplication
import PostgresqlSyntax.Ast.FuncExprCommonSubexpr
import PostgresqlSyntax.Ast.OverClause
import PostgresqlSyntax.Ast.SortClause
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
      TextBuilders.optLexemes
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
          Parser.endHead
          b <- optional (Parsers.space1 *> withinGroupClause)
          c <- optional (Parsers.space1 *> filterClause)
          d <- optional (Parsers.space1 *> parser)
          return (ApplicationFuncExpr a b c d)
      ]
    where
      withinGroupClause = do
        Parsers.keyphrase "within group"
        Parser.endHead
        Parsers.space
        Parsers.inParens parser
      filterClause = do
        Parsers.keyword "filter"
        Parser.endHead
        Parsers.space
        Parsers.inParens (Parsers.keyword "where" *> Parsers.space1 *> parser)

instance Qc.Arbitrary FuncExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ApplicationFuncExpr
          <$> Qc.arbitrary
          <*> Gens.terminatingMaybe Qc.arbitrary
          <*> Gens.terminatingMaybe (Gens.downscale Qc.arbitrary)
          <*> Gens.terminatingMaybe Qc.arbitrary,
        SubexprFuncExpr <$> Qc.arbitrary
      ]
