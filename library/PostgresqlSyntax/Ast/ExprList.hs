module PostgresqlSyntax.Ast.ExprList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- expr_list:
--   | a_expr
--   | expr_list ',' a_expr
-- @
newtype ExprList = ExprList (NonEmpty AExpr)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ExprList where
  toTextBuilder (ExprList a) = commaNonEmpty toTextBuilder a
  parser = ExprList <$> sep1 commaSeparator parser

instance Arbitrary ExprList where
  arbitrary = do
    len <- choose (0, 6)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (ExprList (x :| xs))
