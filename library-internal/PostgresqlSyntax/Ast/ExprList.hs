module PostgresqlSyntax.Ast.ExprList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
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
  toTextBuilder settings (ExprList a) = TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = ExprList <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary ExprList where
  shrink = Qc.genericShrink
  arbitrary = ExprList <$> Gens.nonEmptyUpTo 6 (Gens.downscale Qc.arbitrary)
