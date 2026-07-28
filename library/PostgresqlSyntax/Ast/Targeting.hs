module PostgresqlSyntax.Ast.Targeting where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TargetList
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
        Parser.space1
        AllTargeting . Just <$> parser
      allP = keyword "all" $> AllTargeting Nothing
      distinct = do
        keyword "distinct"
        Parser.space1
        Parser.endHead
        optOn <- optional (onExpressionsClause <* Parser.space1)
        targetList <- parser
        return (DistinctTargeting optOn targetList)
      onExpressionsClause = do
        keyword "on"
        Parser.space1
        Parser.endHead
        ExprList <$> inParens (Parser.sep1 commaSeparator parser)

instance Qc.Arbitrary Targeting where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ NormalTargeting <$> Qc.arbitrary,
        AllTargeting <$> Qc.arbitrary,
        DistinctTargeting <$> Qc.terminatingMaybe Qc.arbitrary <*> Qc.arbitrary
      ]
