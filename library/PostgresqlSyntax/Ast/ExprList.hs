module PostgresqlSyntax.Ast.ExprList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder (ExprList a) = commaNonEmpty toTextBuilder a
  parser = ExprList <$> Parser.sep1 commaSeparator parser

instance Qc.Arbitrary ExprList where
  shrink = Qc.genericShrink
  arbitrary = ExprList <$> Qc.nonEmptyUpTo 6 (Qc.downscale Qc.arbitrary)
