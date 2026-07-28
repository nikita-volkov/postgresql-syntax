module PostgresqlSyntax.Ast.ArrayExprList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.ArrayExpr (ArrayExpr)
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
  toTextBuilder (ArrayExprList a) = TextBuilders.commaNonEmpty toTextBuilder a
  parser = ArrayExprList <$> Parsers.sep1 Parsers.commaSeparator parser

instance Qc.Arbitrary ArrayExprList where
  shrink = Qc.genericShrink
  arbitrary = ArrayExprList <$> Qc.nonEmptyUpTo 100 (Qc.scale (`div` 2) Qc.arbitrary)
