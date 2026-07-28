module PostgresqlSyntax.Ast.ForLockingClause where

import PostgresqlSyntax.Ast.ForLockingItem
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.Helpers.TextBuilders
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
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ItemsForLockingClause <$> Qc.nonEmptyUpTo 7 Qc.arbitrary,
        pure ReadOnlyForLockingClause
      ]
