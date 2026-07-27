module PostgresqlSyntax.Ast.InsertColumnItem where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- insert_column_item:
--   | ColId opt_indirection
-- @
data InsertColumnItem = InsertColumnItem Ident (Maybe Indirection)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InsertColumnItem where
  toTextBuilder (InsertColumnItem a b) = toTextBuilder a <> suffixMaybe toTextBuilder b
  parser = do
    a <- colId
    endHead
    b <- optional (space1 *> parser)
    return (InsertColumnItem a b)

instance Arbitrary InsertColumnItem where
  arbitrary = InsertColumnItem <$> arbitrary <*> scale (`div` 2) arbitrary
