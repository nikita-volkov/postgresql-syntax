module PostgresqlSyntax.Ast.ExtractList where

import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExtractArg
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- extract_list:
--   | extract_arg FROM a_expr
--   | EMPTY
-- @
data ExtractList = ExtractList ExtractArg AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ExtractList where
  toTextBuilder settings (ExtractList a b) = toTextBuilder settings a <> " FROM " <> toTextBuilder settings b
  parser settings = ExtractList <$> parser settings <*> (Parsers.space1 *> Parsers.keyword "from" *> Parsers.space1 *> parser settings)

instance Qc.Arbitrary ExtractList where
  shrink = Qc.genericShrink
  arbitrary = ExtractList <$> arbitrary <*> Gens.downscale arbitrary
