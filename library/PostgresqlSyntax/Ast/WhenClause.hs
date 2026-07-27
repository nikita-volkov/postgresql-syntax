module PostgresqlSyntax.Ast.WhenClause where

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
-- when_clause:
--   |  WHEN a_expr THEN a_expr
-- @
data WhenClause = WhenClause AExpr AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst WhenClause where
  toTextBuilder (WhenClause a b) = "WHEN " <> toTextBuilder a <> " THEN " <> toTextBuilder b
  parser = do
    keyword "when"
    space1
    endHead
    a <- parser
    space1
    keyword "then"
    space1
    b <- parser
    return (WhenClause a b)

instance Arbitrary WhenClause where
  arbitrary = WhenClause <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
