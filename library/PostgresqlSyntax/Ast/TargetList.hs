module PostgresqlSyntax.Ast.TargetList where

import PostgresqlSyntax.Ast.TargetEl
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder (TargetList a) = TextBuilders.commaNonEmpty toTextBuilder a
  parser = TargetList <$> Parsers.sep1 Parsers.commaSeparator parser

instance Qc.Arbitrary TargetList where
  shrink = Qc.genericShrink
  arbitrary = TargetList <$> Qc.nonEmptyUpTo 7 Qc.arbitrary
