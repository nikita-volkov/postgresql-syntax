module PostgresqlSyntax.Ast.IndexParams where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.IndexElem
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- index_params:
--   | index_elem
--   | index_params ',' index_elem
-- @
newtype IndexParams = IndexParams (NonEmpty IndexElem)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst IndexParams where
  toTextBuilder settings (IndexParams a) = TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = IndexParams <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary IndexParams where
  shrink = Qc.genericShrink
  arbitrary = IndexParams <$> Gens.nonEmptyUpTo 4 Qc.arbitrary
