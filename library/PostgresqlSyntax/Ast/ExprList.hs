module PostgresqlSyntax.Ast.ExprList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  arbitrary = do
    len <- Qc.choose (0, 6)
    x <- Qc.scale (`div` 2) Qc.arbitrary
    xs <- Qc.vectorOf len (Qc.scale (`div` 2) Qc.arbitrary)
    pure (ExprList (x :| xs))
