module PostgresqlSyntax.Ast.InExpr where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)
import qualified Text.Megaparsec as Megaparsec

-- |
-- ==== References
-- @
-- in_expr:
--   | select_with_parens
--   | '(' expr_list ')'
-- @
data InExpr
  = SelectInExpr SelectWithParens
  | ExprListInExpr ExprList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InExpr where
  toTextBuilder = \case
    SelectInExpr a -> toTextBuilder a
    ExprListInExpr a -> renderInParens (toTextBuilder a)
  parser =
    (ExprListInExpr <$> parse (Megaparsec.try (toParsec (inParens parser))))
      <|> (SelectInExpr <$> wrapToHead parser)

instance Arbitrary InExpr where
  arbitrary =
    sized $ \n ->
      if n <= 1
        then ExprListInExpr <$> arbitrary
        else
          oneof
            [ SelectInExpr <$> scale (`div` 2) arbitrary,
              ExprListInExpr <$> scale (`div` 2) arbitrary
            ]
