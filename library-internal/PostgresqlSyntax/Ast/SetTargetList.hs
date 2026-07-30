module PostgresqlSyntax.Ast.SetTargetList where

import PostgresqlSyntax.Ast.SetTarget
import qualified PostgresqlSyntax.Helpers.Gens as Gens
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
  toTextBuilder settings (SetTargetList a) = TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = SetTargetList <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary SetTargetList where
  shrink = Qc.genericShrink
  arbitrary = SetTargetList <$> Gens.nonEmptyUpTo 6 Qc.arbitrary
