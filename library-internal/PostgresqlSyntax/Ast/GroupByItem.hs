module PostgresqlSyntax.Ast.GroupByItem where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
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
  toTextBuilder settings = \case
    ExprGroupByItem a -> toTextBuilder settings a
    EmptyGroupingSetGroupByItem -> "()"
    RollupGroupByItem a -> "ROLLUP (" <> toTextBuilder settings a <> ")"
    CubeGroupByItem a -> "CUBE (" <> toTextBuilder settings a <> ")"
    GroupingSetsGroupByItem a -> "GROUPING SETS (" <> TextBuilders.commaNonEmpty (toTextBuilder settings) a <> ")"
  parser settings =
    asum
      [ EmptyGroupingSetGroupByItem <$ (Parsers.char '(' *> Parsers.space *> Parsers.char ')'),
        RollupGroupByItem . ExprList <$> (Parsers.keyword "rollup" *> Parser.endHead *> Parsers.space *> Parsers.inParens (Parsers.sep1 Parsers.commaSeparator (parser settings))),
        CubeGroupByItem . ExprList <$> (Parsers.keyword "cube" *> Parser.endHead *> Parsers.space *> Parsers.inParens (Parsers.sep1 Parsers.commaSeparator (parser settings))),
        GroupingSetsGroupByItem <$> (Parsers.keyphrase "grouping sets" *> Parser.endHead *> Parsers.space *> Parsers.inParens (Parsers.sep1 Parsers.commaSeparator (parser settings))),
        ExprGroupByItem <$> parser settings
      ]

instance Qc.Arbitrary GroupByItem where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then Qc.oneof [ExprGroupByItem <$> Gens.downscale Qc.arbitrary, pure EmptyGroupingSetGroupByItem]
        else
          Qc.oneof
            [ ExprGroupByItem <$> Gens.downscale Qc.arbitrary,
              pure EmptyGroupingSetGroupByItem,
              RollupGroupByItem <$> Qc.arbitrary,
              CubeGroupByItem <$> Qc.arbitrary,
              GroupingSetsGroupByItem <$> Gens.downscale (Gens.nonEmptyUpTo 2 Qc.arbitrary)
            ]
