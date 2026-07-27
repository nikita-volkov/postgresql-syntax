module PostgresqlSyntax.Ast.SetTargetList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SetTarget
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

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
  parser = SetTargetList <$> sep1 commaSeparator parser

instance Arbitrary SetTargetList where
  arbitrary = do
    len <- choose (0, 6)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (SetTargetList (x :| xs))
