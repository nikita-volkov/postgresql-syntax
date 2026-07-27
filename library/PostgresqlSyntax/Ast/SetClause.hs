module PostgresqlSyntax.Ast.SetClause where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SetTarget
import PostgresqlSyntax.Ast.SetTargetList
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- set_clause:
--   | set_target '=' a_expr
--   | '(' set_target_list ')' '=' a_expr
-- @
data SetClause
  = TargetSetClause SetTarget AExpr
  | TargetListSetClause SetTargetList AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SetClause where
  toTextBuilder = \case
    TargetSetClause a b -> toTextBuilder a <> " = " <> toTextBuilder b
    TargetListSetClause a b -> renderInParens (toTextBuilder a) <> " = " <> toTextBuilder b
  parser =
    asum
      [ do
          a <- inParens parser
          space
          char '='
          space
          b <- parser
          return (TargetListSetClause a b),
        do
          a <- parser
          space
          char '='
          space
          b <- parser
          return (TargetSetClause a b)
      ]

instance Arbitrary SetClause where
  arbitrary =
    oneof
      [ TargetSetClause <$> arbitrary <*> scale (`div` 2) arbitrary,
        TargetListSetClause <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
      ]
