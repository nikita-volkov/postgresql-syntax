module PostgresqlSyntax.Ast.TrimList where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.ExprList
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- trim_list:
--   | a_expr FROM expr_list
--   | FROM expr_list
--   | expr_list
-- @
data TrimList
  = ExprFromExprListTrimList AExpr ExprList
  | FromExprListTrimList ExprList
  | ExprListTrimList ExprList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TrimList where
  toTextBuilder = \case
    ExprFromExprListTrimList a b -> toTextBuilder a <> " FROM " <> toTextBuilder b
    FromExprListTrimList a -> "FROM " <> toTextBuilder a
    ExprListTrimList a -> toTextBuilder a
  parser =
    asum
      [ ExprFromExprListTrimList <$> wrapToHead parser <*> (space1 *> keyword "from" *> space1 *> endHead *> parser),
        FromExprListTrimList <$> (keyword "from" *> space1 *> endHead *> parser),
        ExprListTrimList <$> parser
      ]

instance Arbitrary TrimList where
  arbitrary =
    oneof
      [ ExprFromExprListTrimList <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
        FromExprListTrimList <$> scale (`div` 2) arbitrary,
        ExprListTrimList <$> scale (`div` 2) arbitrary
      ]
