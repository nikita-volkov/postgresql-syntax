module PostgresqlSyntax.Ast.ArrayExprList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.ArrayExpr (ArrayExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- array_expr_list:
--   | array_expr
--   | array_expr_list ',' array_expr
-- @
newtype ArrayExprList = ArrayExprList (NonEmpty ArrayExpr)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ArrayExprList where
  toTextBuilder (ArrayExprList a) = commaNonEmpty toTextBuilder a
  parser = ArrayExprList <$> sep1 commaSeparator parser

instance Arbitrary ArrayExprList where
  arbitrary = do
    len <- choose (0, 3)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (ArrayExprList (x :| xs))
