module PostgresqlSyntax.Ast.ExtractList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExtractArg
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

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
  parser = ExtractList <$> parser <*> (space1 *> keyword "from" *> space1 *> parser)

instance Arbitrary ExtractList where
  arbitrary = ExtractList <$> arbitrary <*> scale (`div` 2) arbitrary
