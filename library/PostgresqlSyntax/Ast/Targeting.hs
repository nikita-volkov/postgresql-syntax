module PostgresqlSyntax.Ast.Targeting where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TargetList
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- simple_select:
--   |  SELECT opt_all_clause opt_target_list ...
--   |  SELECT distinct_clause target_list ...
--
-- distinct_clause:
--   |  DISTINCT
--   |  DISTINCT ON '(' expr_list ')'
-- @
data Targeting
  = NormalTargeting TargetList
  | AllTargeting (Maybe TargetList)
  | DistinctTargeting (Maybe ExprList) TargetList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Targeting where
  toTextBuilder = \case
    NormalTargeting a -> toTextBuilder a
    AllTargeting a -> "ALL" <> suffixMaybe toTextBuilder a
    DistinctTargeting a b -> "DISTINCT" <> suffixMaybe onExpressionsClause a <> " " <> toTextBuilder b
    where
      onExpressionsClause a = "ON (" <> toTextBuilder a <> ")"
  parser = distinct <|> allWithTargetList <|> allP <|> normal
    where
      normal = NormalTargeting <$> parser
      allWithTargetList = do
        keyword "all"
        space1
        AllTargeting . Just <$> parser
      allP = keyword "all" $> AllTargeting Nothing
      distinct = do
        keyword "distinct"
        space1
        endHead
        optOn <- optional (onExpressionsClause <* space1)
        targetList <- parser
        return (DistinctTargeting optOn targetList)
      onExpressionsClause = do
        keyword "on"
        space1
        endHead
        ExprList <$> inParens (sep1 commaSeparator parser)

instance Arbitrary Targeting where
  arbitrary =
    oneof
      [ NormalTargeting <$> arbitrary,
        AllTargeting <$> arbitrary,
        DistinctTargeting <$> scale (`div` 2) arbitrary <*> arbitrary
      ]
