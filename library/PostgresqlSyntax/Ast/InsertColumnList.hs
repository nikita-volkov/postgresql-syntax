module PostgresqlSyntax.Ast.InsertColumnList where

import PostgresqlSyntax.Ast.InsertColumnItem
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- insert_column_list:
--   | insert_column_item
--   | insert_column_list ',' insert_column_item
-- @
newtype InsertColumnList = InsertColumnList (NonEmpty InsertColumnItem)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InsertColumnList where
  toTextBuilder (InsertColumnList a) = TextBuilders.commaNonEmpty toTextBuilder a
  parser = InsertColumnList <$> Parsers.sep1 Parsers.commaSeparator parser

instance Qc.Arbitrary InsertColumnList where
  shrink = Qc.genericShrink
  arbitrary = InsertColumnList <$> Qc.nonEmptyUpTo 6 Qc.arbitrary
