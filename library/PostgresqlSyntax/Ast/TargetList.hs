module PostgresqlSyntax.Ast.TargetList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TargetEl
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder (TargetList a) = commaNonEmpty toTextBuilder a
  parser = TargetList <$> Parser.sep1 commaSeparator parser

instance Qc.Arbitrary TargetList where
  shrink = Qc.genericShrink
  arbitrary = TargetList <$> Qc.nonEmptyUpTo 7 Qc.arbitrary
