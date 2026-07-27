module PostgresqlSyntax.Ast.InsertColumnList where

import PostgresqlSyntax.Ast.InsertColumnItem
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  toTextBuilder (InsertColumnList a) = commaNonEmpty toTextBuilder a
  parser = InsertColumnList <$> Parser.sep1 commaSeparator parser

instance Qc.Arbitrary InsertColumnList where
  arbitrary = do
    len <- Qc.choose (0, 6)
    x <- Qc.scale (`div` 2) Qc.arbitrary
    xs <- Qc.vectorOf len (Qc.scale (`div` 2) Qc.arbitrary)
    pure (InsertColumnList (x :| xs))
