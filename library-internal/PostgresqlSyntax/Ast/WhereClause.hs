module PostgresqlSyntax.Ast.WhereClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- where_clause:
--   |  WHERE a_expr
--   |  /*EMPTY*/
-- @
newtype WhereClause = WhereClause AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst WhereClause where
  toTextBuilder settings (WhereClause a) = "WHERE " <> toTextBuilder settings a
  parser settings = do
    Parsers.keyword "where"
    Parsers.space1
    Parser.endHead
    WhereClause <$> parser settings

instance Qc.Arbitrary WhereClause where
  shrink = Qc.genericShrink
  arbitrary = WhereClause <$> Gens.downscale Qc.arbitrary
