module PostgresqlSyntax.Ast.WhenClause where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
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
    Parsers.keyword "when"
    Parsers.space1
    Parser.endHead
    a <- parser
    Parsers.space1
    Parsers.keyword "then"
    Parsers.space1
    b <- parser
    return (WhenClause a b)

instance Qc.Arbitrary WhenClause where
  shrink = Qc.genericShrink
  arbitrary = WhenClause <$> Gens.downscale arbitrary <*> Gens.downscale arbitrary
