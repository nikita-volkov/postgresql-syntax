module PostgresqlSyntax.Ast.JoinType where

import HeadedMegaparsec hiding (string)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, option, some, try)

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
  toTextBuilder = \case
    FullJoinType a -> "FULL" <> if a then " OUTER" else ""
    LeftJoinType a -> "LEFT" <> if a then " OUTER" else ""
    RightJoinType a -> "RIGHT" <> if a then " OUTER" else ""
    InnerJoinType -> "INNER"
  parser =
    asum
      [ do
          keyword "full"
          endHead
          outer <- outerAfterSpace
          return (FullJoinType outer),
        do
          keyword "left"
          endHead
          outer <- outerAfterSpace
          return (LeftJoinType outer),
        do
          keyword "right"
          endHead
          outer <- outerAfterSpace
          return (RightJoinType outer),
        keyword "inner" $> InnerJoinType
      ]
    where
      outerAfterSpace = (space1 *> keyword "outer") $> True <|> pure False

instance Arbitrary JoinType where
  arbitrary =
    oneof
      [ FullJoinType <$> arbitrary,
        LeftJoinType <$> arbitrary,
        RightJoinType <$> arbitrary,
        pure InnerJoinType
      ]
