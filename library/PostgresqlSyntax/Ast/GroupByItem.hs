module PostgresqlSyntax.Ast.GroupByItem where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
    GroupingSetsGroupByItem a -> "GROUPING SETS (" <> TextBuilders.commaNonEmpty toTextBuilder a <> ")"
  parser =
    asum
      [ EmptyGroupingSetGroupByItem <$ (Parsers.char '(' *> Parsers.space *> Parsers.char ')'),
        RollupGroupByItem . ExprList <$> (Parsers.keyword "rollup" *> Parser.endHead *> Parsers.space *> Parsers.inParens (Parsers.sep1 Parsers.commaSeparator parser)),
        CubeGroupByItem . ExprList <$> (Parsers.keyword "cube" *> Parser.endHead *> Parsers.space *> Parsers.inParens (Parsers.sep1 Parsers.commaSeparator parser)),
        GroupingSetsGroupByItem <$> (Parsers.keyphrase "grouping sets" *> Parser.endHead *> Parsers.space *> Parsers.inParens (Parsers.sep1 Parsers.commaSeparator parser)),
        ExprGroupByItem <$> parser
      ]

instance Qc.Arbitrary GroupByItem where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then Qc.oneof [ExprGroupByItem <$> Qc.scale (`div` 2) Qc.arbitrary, pure EmptyGroupingSetGroupByItem]
        else
          Qc.oneof
            [ ExprGroupByItem <$> Qc.scale (`div` 2) Qc.arbitrary,
              pure EmptyGroupingSetGroupByItem,
              RollupGroupByItem <$> Qc.scale (`div` 2) Qc.arbitrary,
              CubeGroupByItem <$> Qc.scale (`div` 2) Qc.arbitrary,
              GroupingSetsGroupByItem <$> Qc.nonEmptyUpTo 2 (Qc.scale (`div` 4) Qc.arbitrary)
            ]
