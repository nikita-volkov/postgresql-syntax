module PostgresqlSyntax.Ast.OptOrdinality where

import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- opt_ordinality:
--   | WITH_LA ORDINALITY
--   | EMPTY
-- @
newtype OptOrdinality = OptOrdinality Bool
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OptOrdinality where
  toTextBuilder _settings (OptOrdinality a) = if a then "WITH ORDINALITY" else mempty
  parser _settings = OptOrdinality <$> Parsers.trueIfPresent (Parsers.keyword "with" *> Parsers.space1 *> Parsers.keyword "ordinality")

instance Qc.Arbitrary OptOrdinality where
  shrink = Qc.genericShrink
  arbitrary = OptOrdinality <$> arbitrary
