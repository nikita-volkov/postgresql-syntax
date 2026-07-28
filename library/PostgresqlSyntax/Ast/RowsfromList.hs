module PostgresqlSyntax.Ast.RowsfromList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.RowsfromItem
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder (RowsfromList a) = commaNonEmpty toTextBuilder a
  parser = RowsfromList <$> Parser.sep1 commaSeparator parser

instance Qc.Arbitrary RowsfromList where
  arbitrary = RowsfromList <$> Qc.nonEmptyUpTo 7 Qc.arbitrary
