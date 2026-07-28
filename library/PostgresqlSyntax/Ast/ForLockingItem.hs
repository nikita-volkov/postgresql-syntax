module PostgresqlSyntax.Ast.ForLockingItem where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ForLockingStrength
import PostgresqlSyntax.Ast.QualifiedName
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
    TextBuilders.optLexemes
      [ Just (toTextBuilder a),
        fmap lockedRelsList b,
        fmap nowaitOrSkip c
      ]
    where
      lockedRelsList a' = "OF " <> TextBuilders.commaNonEmpty toTextBuilder a'
      nowaitOrSkip = bool "NOWAIT" "SKIP LOCKED"
  parser = do
    strength <- parser
    rels <- optional $ Parsers.space1 *> Parsers.keyword "of" *> Parsers.space1 *> Parser.endHead *> Parsers.sep1 Parsers.commaSeparator parser
    nowaitOrSkip <- optional (Parsers.space1 *> nowaitOrSkip)
    return (ForLockingItem strength rels nowaitOrSkip)
    where
      nowaitOrSkip = False <$ Parsers.keyword "nowait" <|> True <$ Parsers.keyphrase "skip locked"

instance Qc.Arbitrary ForLockingItem where
  shrink = Qc.genericShrink
  arbitrary = ForLockingItem <$> arbitrary <*> Qc.terminatingMaybe arbitrary <*> arbitrary
