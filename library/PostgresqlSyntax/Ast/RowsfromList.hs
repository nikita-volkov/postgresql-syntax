module PostgresqlSyntax.Ast.RowsfromList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.RowsfromItem
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- rowsfrom_list:
--   | rowsfrom_item
--   | rowsfrom_list ',' rowsfrom_item
-- @
newtype RowsfromList = RowsfromList (NonEmpty RowsfromItem)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst RowsfromList where
  toTextBuilder (RowsfromList a) = commaNonEmpty toTextBuilder a
  parser = RowsfromList <$> sep1 commaSeparator parser

instance Arbitrary RowsfromList where
  arbitrary = do
    len <- choose (0, 7)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (RowsfromList (x :| xs))
