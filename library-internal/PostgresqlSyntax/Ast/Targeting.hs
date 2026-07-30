module PostgresqlSyntax.Ast.Targeting where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.TargetList
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
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
  toTextBuilder settings = \case
    NormalTargeting a -> toTextBuilder settings a
    AllTargeting a -> "ALL" <> TextBuilders.suffixMaybe (toTextBuilder settings) a
    DistinctTargeting a b -> "DISTINCT" <> TextBuilders.suffixMaybe onExpressionsClause a <> " " <> toTextBuilder settings b
    where
      onExpressionsClause a = "ON (" <> toTextBuilder settings a <> ")"
  parser settings = distinct <|> allWithTargetList <|> allP <|> normal
    where
      normal = NormalTargeting <$> parser settings
      allWithTargetList = do
        Parsers.keyword "all"
        Parsers.space1
        AllTargeting . Just <$> parser settings
      allP = Parsers.keyword "all" $> AllTargeting Nothing
      distinct = do
        Parsers.keyword "distinct"
        Parsers.space1
        Parser.endHead
        optOn <- optional (onExpressionsClause <* Parsers.space1)
        targetList <- parser settings
        return (DistinctTargeting optOn targetList)
      onExpressionsClause = do
        Parsers.keyword "on"
        Parsers.space1
        Parser.endHead
        ExprList <$> Parsers.inParens (Parsers.sep1 Parsers.commaSeparator (parser settings))

instance Qc.Arbitrary Targeting where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ NormalTargeting <$> Qc.arbitrary,
        AllTargeting <$> Qc.arbitrary,
        DistinctTargeting <$> Gens.terminatingMaybe Qc.arbitrary <*> Qc.arbitrary
      ]
