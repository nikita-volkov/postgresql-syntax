module PostgresqlSyntax.Ast.JoinQual where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- join_qual:
--   |  USING '(' name_list ')'
--   |  ON a_expr
-- @
data JoinQual
  = UsingJoinQual (NonEmpty Ident)
  | OnJoinQual AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst JoinQual where
  toTextBuilder = \case
    UsingJoinQual a -> "USING (" <> commaNonEmpty toTextBuilder a <> ")"
    OnJoinQual a -> "ON " <> toTextBuilder a
  parser =
    asum
      [ keyword "using" *> space1 *> inParens (sep1 commaSeparator colId) <&> UsingJoinQual,
        keyword "on" *> space1 *> parser <&> OnJoinQual
      ]

instance Arbitrary JoinQual where
  arbitrary =
    oneof
      [ do
          len <- choose (0, 7)
          x <- arbitrary
          xs <- vectorOf len arbitrary
          pure (UsingJoinQual (x :| xs)),
        OnJoinQual <$> scale (`div` 2) arbitrary
      ]
