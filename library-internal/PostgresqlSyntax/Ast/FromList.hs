module PostgresqlSyntax.Ast.FromList where

import PostgresqlSyntax.Ast.TableRef
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- from_list:
--   |  table_ref
--   |  from_list ',' table_ref
-- @
newtype FromList = FromList (NonEmpty TableRef)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FromList where
  toTextBuilder settings (FromList a) = TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = FromList <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary FromList where
  shrink = Qc.genericShrink
  arbitrary = FromList <$> Gens.nonEmptyUpTo 6 Qc.arbitrary
