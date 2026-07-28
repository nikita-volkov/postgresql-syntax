module PostgresqlSyntax.Ast.TableFuncElementList where

import PostgresqlSyntax.Ast.TableFuncElement
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
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
  toTextBuilder (TableFuncElementList a) = TextBuilders.commaNonEmpty toTextBuilder a
  parser = TableFuncElementList <$> Parsers.sep1 Parsers.commaSeparator parser

instance Qc.Arbitrary TableFuncElementList where
  shrink = Qc.genericShrink
  arbitrary = TableFuncElementList <$> Qc.nonEmptyUpTo 6 Qc.arbitrary
