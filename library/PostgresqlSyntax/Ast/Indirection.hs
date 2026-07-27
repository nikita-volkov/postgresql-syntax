module PostgresqlSyntax.Ast.Indirection where

import Control.Applicative.Combinators.NonEmpty (some)
import PostgresqlSyntax.Ast.IndirectionEl
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (some)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- indirection:
--   |  indirection_el
--   |  indirection indirection_el
-- @
newtype Indirection = Indirection (NonEmpty IndirectionEl)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Indirection where
  toTextBuilder (Indirection a) = foldMap toTextBuilder a
  parser = Indirection <$> some parser

instance Arbitrary Indirection where
  arbitrary = do
    len <- choose (0, 2)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (Indirection (x :| xs))
