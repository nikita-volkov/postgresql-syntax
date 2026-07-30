module PostgresqlSyntax.Ast.HavingClause where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- having_clause:
--   |  HAVING a_expr
--   |  /*EMPTY*/
-- @
newtype HavingClause = HavingClause AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst HavingClause where
  toTextBuilder settings (HavingClause a) = "HAVING " <> toTextBuilder settings a
  parser settings = do
    Parsers.keyword "having"
    Parser.endHead
    Parsers.space1
    HavingClause <$> parser settings

instance Qc.Arbitrary HavingClause where
  shrink = Qc.genericShrink
  arbitrary = HavingClause <$> Gens.downscale Qc.arbitrary
