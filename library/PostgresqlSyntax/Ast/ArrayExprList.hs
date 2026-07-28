module PostgresqlSyntax.Ast.ArrayExprList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.ArrayExpr (ArrayExpr)
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  shrink = Qc.genericShrink
  arbitrary = ArrayExprList <$> Qc.nonEmptyUpTo 100 (Qc.scale (`div` 2) Qc.arbitrary)
