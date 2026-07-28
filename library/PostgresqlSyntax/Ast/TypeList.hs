module PostgresqlSyntax.Ast.TypeList where

import PostgresqlSyntax.Ast.Typename
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
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
  toTextBuilder (TypeList a) = TextBuilders.commaNonEmpty toTextBuilder a
  parser = TypeList <$> Parsers.sep1 Parsers.commaSeparator parser

instance Qc.Arbitrary TypeList where
  shrink = Qc.genericShrink
  arbitrary = TypeList <$> Qc.nonEmptyUpTo 6 Qc.arbitrary
