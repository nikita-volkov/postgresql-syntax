module PostgresqlSyntax.Ast.IndexParams where

import PostgresqlSyntax.Ast.IndexElem
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- index_params:
--   | index_elem
--   | index_params ',' index_elem
-- @
newtype IndexParams = IndexParams (NonEmpty IndexElem)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst IndexParams where
  toTextBuilder (IndexParams a) = commaNonEmpty toTextBuilder a
  parser = IndexParams <$> sep1 commaSeparator parser

instance Arbitrary IndexParams where
  arbitrary = do
    len <- choose (0, 4)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (IndexParams (x :| xs))
