module PostgresqlSyntax.Ast.ExprList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder (ExprList a) = TextBuilders.commaNonEmpty toTextBuilder a
  parser = ExprList <$> Parsers.sep1 Parsers.commaSeparator parser

instance Qc.Arbitrary ExprList where
  shrink = Qc.genericShrink
  arbitrary = ExprList <$> Qc.nonEmptyUpTo 6 Qc.arbitrary
