module PostgresqlSyntax.Ast.RowsfromList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.RowsfromItem
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  arbitrary = do
    len <- Qc.choose (0, 7)
    x <- Qc.scale (`div` 2) Qc.arbitrary
    xs <- Qc.vectorOf len (Qc.scale (`div` 2) Qc.arbitrary)
    pure (RowsfromList (x :| xs))
