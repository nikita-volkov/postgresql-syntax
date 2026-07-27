module PostgresqlSyntax.Ast.ForLockingItem where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.ForLockingStrength
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.QualifiedName
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- for_locking_item:
--   | for_locking_strength locked_rels_list opt_nowait_or_skip
-- locked_rels_list:
--   | OF qualified_name_list
--   | EMPTY
-- opt_nowait_or_skip:
--   | NOWAIT
--   | SKIP LOCKED
--   | EMPTY
-- @
data ForLockingItem = ForLockingItem ForLockingStrength (Maybe (NonEmpty QualifiedName)) (Maybe Bool)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ForLockingItem where
  toTextBuilder (ForLockingItem a b c) =
    optLexemes
      [ Just (toTextBuilder a),
        fmap lockedRelsList b,
        fmap nowaitOrSkip c
      ]
    where
      lockedRelsList a' = "OF " <> commaNonEmpty toTextBuilder a'
      nowaitOrSkip = bool "NOWAIT" "SKIP LOCKED"
  parser = do
    strength <- parser
    rels <- optional $ space1 *> keyword "of" *> space1 *> endHead *> sep1 commaSeparator parser
    nowaitOrSkip <- optional (space1 *> nowaitOrSkip)
    return (ForLockingItem strength rels nowaitOrSkip)
    where
      nowaitOrSkip = False <$ keyword "nowait" <|> True <$ keyphrase "skip locked"

instance Arbitrary ForLockingItem where
  arbitrary = ForLockingItem <$> arbitrary <*> scale (`div` 2) arbitrary <*> arbitrary
