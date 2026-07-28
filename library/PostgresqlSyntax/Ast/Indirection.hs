module PostgresqlSyntax.Ast.Indirection where

import Control.Applicative.Combinators.NonEmpty (some)
import PostgresqlSyntax.Ast.IndirectionEl
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (some)
import qualified Test.QuickCheck as Qc

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

instance Qc.Arbitrary Indirection where
  shrink = Qc.genericShrink
  arbitrary = Indirection <$> Gens.nonEmptyUpTo 4 Qc.arbitrary
