module PostgresqlSyntax.Ast.InsertTarget where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.QualifiedName
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- insert_target:
--   | qualified_name
--   | qualified_name AS ColId
-- @
data InsertTarget = InsertTarget QualifiedName (Maybe Ident)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InsertTarget where
  toTextBuilder (InsertTarget a b) = toTextBuilder a <> foldMap (mappend " AS " . toTextBuilder) b
  parser = do
    a <- parser
    endHead
    b <- optional (space1 *> keyword "as" *> space1 *> endHead *> colId)
    return (InsertTarget a b)

instance Arbitrary InsertTarget where
  arbitrary = InsertTarget <$> arbitrary <*> scale (`div` 2) arbitrary
