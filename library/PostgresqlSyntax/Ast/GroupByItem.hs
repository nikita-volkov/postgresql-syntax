module PostgresqlSyntax.Ast.GroupByItem where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- group_by_item:
--   |  a_expr
--   |  empty_grouping_set
--   |  cube_clause
--   |  rollup_clause
--   |  grouping_sets_clause
-- empty_grouping_set:
--   |  '(' ')'
-- rollup_clause:
--   |  ROLLUP '(' expr_list ')'
-- cube_clause:
--   |  CUBE '(' expr_list ')'
-- grouping_sets_clause:
--   |  GROUPING SETS '(' group_by_list ')'
-- @
data GroupByItem
  = ExprGroupByItem AExpr
  | EmptyGroupingSetGroupByItem
  | RollupGroupByItem ExprList
  | CubeGroupByItem ExprList
  | GroupingSetsGroupByItem (NonEmpty GroupByItem)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst GroupByItem where
  toTextBuilder = \case
    ExprGroupByItem a -> toTextBuilder a
    EmptyGroupingSetGroupByItem -> "()"
    RollupGroupByItem a -> "ROLLUP (" <> toTextBuilder a <> ")"
    CubeGroupByItem a -> "CUBE (" <> toTextBuilder a <> ")"
    GroupingSetsGroupByItem a -> "GROUPING SETS (" <> commaNonEmpty toTextBuilder a <> ")"
  parser =
    asum
      [ EmptyGroupingSetGroupByItem <$ (char '(' *> space *> char ')'),
        RollupGroupByItem . ExprList <$> (keyword "rollup" *> endHead *> space *> inParens (sep1 commaSeparator parser)),
        CubeGroupByItem . ExprList <$> (keyword "cube" *> endHead *> space *> inParens (sep1 commaSeparator parser)),
        GroupingSetsGroupByItem <$> (keyphrase "grouping sets" *> endHead *> space *> inParens (sep1 commaSeparator parser)),
        ExprGroupByItem <$> parser
      ]

instance Arbitrary GroupByItem where
  arbitrary =
    sized $ \n ->
      if n <= 1
        then oneof [ExprGroupByItem <$> scale (`div` 2) arbitrary, pure EmptyGroupingSetGroupByItem]
        else
          oneof
            [ ExprGroupByItem <$> scale (`div` 2) arbitrary,
              pure EmptyGroupingSetGroupByItem,
              RollupGroupByItem <$> scale (`div` 2) arbitrary,
              CubeGroupByItem <$> scale (`div` 2) arbitrary,
              GroupingSetsGroupByItem <$> do
                len <- choose (0, 2)
                x <- scale (`div` 4) arbitrary
                xs <- vectorOf len (scale (`div` 4) arbitrary)
                pure (x :| xs)
            ]
