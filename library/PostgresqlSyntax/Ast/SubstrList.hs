module PostgresqlSyntax.Ast.SubstrList where

import HeadedMegaparsec
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.SubstrListFromFor
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- substr_list:
--   | a_expr substr_from substr_for
--   | a_expr substr_for substr_from
--   | a_expr substr_from
--   | a_expr substr_for
--   | expr_list
--   | EMPTY
-- @
data SubstrList
  = ExprSubstrList AExpr SubstrListFromFor
  | ExprListSubstrList ExprList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SubstrList where
  toTextBuilder = \case
    ExprSubstrList a b -> toTextBuilder a <> " " <> toTextBuilder b
    ExprListSubstrList a -> toTextBuilder a
  parser =
    asum
      [ ExprSubstrList <$> wrapToHead parser <*> (space1 *> parser),
        ExprListSubstrList <$> parser
      ]

instance Arbitrary SubstrList where
  arbitrary =
    oneof
      [ ExprSubstrList <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
        ExprListSubstrList <$> scale (`div` 2) arbitrary
      ]
