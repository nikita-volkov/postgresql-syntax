module PostgresqlSyntax.Ast.TypeList where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.Typename
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- type_list:
--   | Typename
--   | type_list ',' Typename
-- @
newtype TypeList = TypeList (NonEmpty Typename)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TypeList where
  toTextBuilder settings (TypeList a) = TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = TypeList <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary TypeList where
  shrink = Qc.genericShrink
  arbitrary = TypeList <$> Gens.nonEmptyUpTo 6 Qc.arbitrary
