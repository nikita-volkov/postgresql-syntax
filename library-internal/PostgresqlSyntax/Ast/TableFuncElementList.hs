module PostgresqlSyntax.Ast.TableFuncElementList where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.TableFuncElement
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- TableFuncElementList:
--   | TableFuncElement
--   | TableFuncElementList ',' TableFuncElement
-- @
newtype TableFuncElementList = TableFuncElementList (NonEmpty TableFuncElement)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TableFuncElementList where
  toTextBuilder settings (TableFuncElementList a) = TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = TableFuncElementList <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary TableFuncElementList where
  shrink = Qc.genericShrink
  arbitrary = TableFuncElementList <$> Gens.nonEmptyUpTo 6 Qc.arbitrary
