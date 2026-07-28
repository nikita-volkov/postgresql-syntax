module PostgresqlSyntax.Ast.IndexParams where

import PostgresqlSyntax.Ast.IndexElem
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
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
  toTextBuilder (IndexParams a) = TextBuilders.commaNonEmpty toTextBuilder a
  parser = IndexParams <$> Parsers.sep1 Parsers.commaSeparator parser

instance Qc.Arbitrary IndexParams where
  shrink = Qc.genericShrink
  arbitrary = IndexParams <$> Qc.nonEmptyUpTo 4 Qc.arbitrary
