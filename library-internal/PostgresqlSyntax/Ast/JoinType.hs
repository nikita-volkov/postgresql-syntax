module PostgresqlSyntax.Ast.JoinType where

import qualified HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- | FULL join_outer
-- | LEFT join_outer
-- | RIGHT join_outer
-- | INNER_P
-- @
data JoinType
  = FullJoinType Bool
  | LeftJoinType Bool
  | RightJoinType Bool
  | InnerJoinType
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst JoinType where
  toTextBuilder settings = \case
    FullJoinType a -> "FULL" <> if a then " OUTER" else ""
    LeftJoinType a -> "LEFT" <> if a then " OUTER" else ""
    RightJoinType a -> "RIGHT" <> if a then " OUTER" else ""
    InnerJoinType -> "INNER"
  parser settings =
    asum
      [ do
          Parsers.keyword "full"
          Parser.endHead
          outer <- outerAfterSpace
          return (FullJoinType outer),
        do
          Parsers.keyword "left"
          Parser.endHead
          outer <- outerAfterSpace
          return (LeftJoinType outer),
        do
          Parsers.keyword "right"
          Parser.endHead
          outer <- outerAfterSpace
          return (RightJoinType outer),
        Parsers.keyword "inner" $> InnerJoinType
      ]
    where
      outerAfterSpace = (Parsers.space1 *> Parsers.keyword "outer") $> True <|> pure False

instance Qc.Arbitrary JoinType where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ FullJoinType <$> Qc.arbitrary,
        LeftJoinType <$> Qc.arbitrary,
        RightJoinType <$> Qc.arbitrary,
        pure InnerJoinType
      ]
