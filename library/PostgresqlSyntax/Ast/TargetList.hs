module PostgresqlSyntax.Ast.TargetList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TargetEl
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

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
  parser = TargetList <$> sep1 commaSeparator parser

instance Arbitrary TargetList where
  arbitrary = do
    len <- choose (0, 7)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (TargetList (x :| xs))
