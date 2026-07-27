module PostgresqlSyntax.Ast.ArrayExprList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.ArrayExpr (ArrayExpr)
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  parser = ArrayExprList <$> Parser.sep1 commaSeparator parser

instance Qc.Arbitrary ArrayExprList where
  arbitrary = do
    len <- Qc.choose (0, 3)
    x <- Qc.scale (`div` 2) Qc.arbitrary
    xs <- Qc.vectorOf len (Qc.scale (`div` 2) Qc.arbitrary)
    pure (ArrayExprList (x :| xs))
