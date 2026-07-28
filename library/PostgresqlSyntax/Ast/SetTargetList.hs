module PostgresqlSyntax.Ast.SetTargetList where

import PostgresqlSyntax.Ast.SetTarget
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- set_target_list:
--   | set_target
--   | set_target_list ',' set_target
-- @
newtype SetTargetList = SetTargetList (NonEmpty SetTarget)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SetTargetList where
  toTextBuilder (SetTargetList a) = TextBuilders.commaNonEmpty toTextBuilder a
  parser = SetTargetList <$> Parsers.sep1 Parsers.commaSeparator parser

instance Qc.Arbitrary SetTargetList where
  shrink = Qc.genericShrink
  arbitrary = SetTargetList <$> Qc.nonEmptyUpTo 6 Qc.arbitrary
