module PostgresqlSyntax.Ast.TargetList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TargetEl
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  arbitrary = do
    len <- Qc.choose (0, 7)
    x <- Qc.scale (`div` 2) Qc.arbitrary
    xs <- Qc.vectorOf len (Qc.scale (`div` 2) Qc.arbitrary)
    pure (TargetList (x :| xs))
