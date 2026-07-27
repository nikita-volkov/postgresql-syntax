module PostgresqlSyntax.Ast.ForLockingClause where

import PostgresqlSyntax.Ast.ForLockingItem
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- for_locking_clause:
--   | for_locking_items
--   | FOR READ ONLY
-- for_locking_items:
--   | for_locking_item
--   | for_locking_items for_locking_item
-- @
data ForLockingClause
  = ItemsForLockingClause (NonEmpty ForLockingItem)
  | ReadOnlyForLockingClause
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ForLockingClause where
  toTextBuilder = \case
    ItemsForLockingClause a -> spaceNonEmpty toTextBuilder a
    ReadOnlyForLockingClause -> "FOR READ ONLY"
  parser = readOnly <|> items
    where
      readOnly = ReadOnlyForLockingClause <$ keyphrase "for read only"
      items = ItemsForLockingClause <$> sep1 space1 parser

instance Arbitrary ForLockingClause where
  arbitrary =
    oneof
      [ ItemsForLockingClause <$> do
          len <- choose (0, 7)
          x <- scale (`div` 2) arbitrary
          xs <- vectorOf len (scale (`div` 2) arbitrary)
          pure (x :| xs),
        pure ReadOnlyForLockingClause
      ]
