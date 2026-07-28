module PostgresqlSyntax.Ast.WhenClause where

import qualified HeadedMegaparsec as Parser
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
-- when_clause:
--   |  WHEN a_expr THEN a_expr
-- @
data WhenClause = WhenClause AExpr AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst WhenClause where
  toTextBuilder (WhenClause a b) = "WHEN " <> toTextBuilder a <> " THEN " <> toTextBuilder b
  parser = do
    keyword "when"
    Parser.space1
    Parser.endHead
    a <- parser
    Parser.space1
    keyword "then"
    Parser.space1
    b <- parser
    return (WhenClause a b)

instance Qc.Arbitrary WhenClause where
  shrink = Qc.genericShrink
  arbitrary = WhenClause <$> Qc.downscale arbitrary <*> Qc.downscale arbitrary
