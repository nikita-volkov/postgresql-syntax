module PostgresqlSyntax.Ast.SubstrListFromFor where

import HeadedMegaparsec
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
--   | a_expr substr_from substr_for
--   | a_expr substr_for substr_from
--   | a_expr substr_from
--   | a_expr substr_for
-- @
data SubstrListFromFor
  = FromForSubstrListFromFor AExpr AExpr
  | ForFromSubstrListFromFor AExpr AExpr
  | FromSubstrListFromFor AExpr
  | ForSubstrListFromFor AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SubstrListFromFor where
  toTextBuilder = \case
    FromForSubstrListFromFor a b -> "FROM " <> toTextBuilder a <> " FOR " <> toTextBuilder b
    ForFromSubstrListFromFor a b -> "FOR " <> toTextBuilder a <> " FROM " <> toTextBuilder b
    FromSubstrListFromFor a -> "FROM " <> toTextBuilder a
    ForSubstrListFromFor a -> "FOR " <> toTextBuilder a
  parser =
    asum
      [ do
          a <- substrFrom
          asum
            [ do
                b <- space1 *> substrFor
                return (FromForSubstrListFromFor a b),
              return (FromSubstrListFromFor a)
            ],
        do
          a <- substrFor
          asum
            [ do
                b <- space1 *> substrFrom
                return (ForFromSubstrListFromFor a b),
              return (ForSubstrListFromFor a)
            ]
      ]
    where
      substrFrom = keyword "from" *> space1 *> endHead *> parser
      substrFor = keyword "for" *> space1 *> endHead *> parser

instance Arbitrary SubstrListFromFor where
  arbitrary =
    oneof
      [ FromForSubstrListFromFor <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
        ForFromSubstrListFromFor <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
        FromSubstrListFromFor <$> scale (`div` 2) arbitrary,
        ForSubstrListFromFor <$> scale (`div` 2) arbitrary
      ]
