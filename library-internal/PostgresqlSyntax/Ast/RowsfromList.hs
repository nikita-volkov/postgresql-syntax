module PostgresqlSyntax.Ast.RowsfromList where

import PostgresqlSyntax.Ast.RowsfromItem
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder (RowsfromList a) = TextBuilders.commaNonEmpty toTextBuilder a
  parser = RowsfromList <$> Parsers.sep1 Parsers.commaSeparator parser

instance Qc.Arbitrary RowsfromList where
  shrink = Qc.genericShrink
  arbitrary = RowsfromList <$> Gens.nonEmptyUpTo 7 Qc.arbitrary
