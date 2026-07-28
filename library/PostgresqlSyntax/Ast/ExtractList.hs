module PostgresqlSyntax.Ast.ExtractList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExtractArg
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
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
  toTextBuilder (ExtractList a b) = toTextBuilder a <> " FROM " <> toTextBuilder b
  parser = ExtractList <$> parser <*> (Parsers.space1 *> Parsers.keyword "from" *> Parsers.space1 *> parser)

instance Qc.Arbitrary ExtractList where
  shrink = Qc.genericShrink
  arbitrary = ExtractList <$> arbitrary <*> Qc.downscale arbitrary
