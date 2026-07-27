module PostgresqlSyntax.Ast.TypeList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.Typename
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- type_list:
--   | Typename
--   | type_list ',' Typename
-- @
newtype TypeList = TypeList (NonEmpty Typename)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TypeList where
  toTextBuilder (TypeList a) = commaNonEmpty toTextBuilder a
  parser = TypeList <$> sep1 commaSeparator parser

instance Arbitrary TypeList where
  arbitrary = do
    len <- choose (0, 6)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (TypeList (x :| xs))
