module PostgresqlSyntax.Ast.SetTargetList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SetTarget
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder (SetTargetList a) = commaNonEmpty toTextBuilder a
  parser = SetTargetList <$> Parser.sep1 commaSeparator parser

instance Qc.Arbitrary SetTargetList where
  arbitrary = SetTargetList <$> Qc.nonEmptyUpTo 6 Qc.arbitrary
