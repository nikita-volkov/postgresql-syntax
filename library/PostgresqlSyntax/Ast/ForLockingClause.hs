module PostgresqlSyntax.Ast.ForLockingClause where

import PostgresqlSyntax.Ast.ForLockingItem
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
      items = ItemsForLockingClause <$> Parser.sep1 Parser.space1 parser

instance Qc.Arbitrary ForLockingClause where
  arbitrary =
    Qc.oneof
      [ ItemsForLockingClause <$> do
          len <- Qc.choose (0, 7)
          x <- Qc.scale (`div` 2) Qc.arbitrary
          xs <- Qc.vectorOf len (Qc.scale (`div` 2) Qc.arbitrary)
          pure (x :| xs),
        pure ReadOnlyForLockingClause
      ]
