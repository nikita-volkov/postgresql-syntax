module PostgresqlSyntax.Ast.TargetList where

import PostgresqlSyntax.Ast.TargetEl
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- target_list:
--   | target_el
--   | target_list ',' target_el
-- @
newtype TargetList = TargetList (NonEmpty TargetEl)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TargetList where
  toTextBuilder settings (TargetList a) = TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = TargetList <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary TargetList where
  shrink = Qc.genericShrink
  arbitrary = TargetList <$> Gens.nonEmptyUpTo 7 Qc.arbitrary
