module PostgresqlSyntax.Ast.TableFuncElementList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TableFuncElement
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

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
  toTextBuilder (TableFuncElementList a) = commaNonEmpty toTextBuilder a
  parser = TableFuncElementList <$> sep1 commaSeparator parser

instance Arbitrary TableFuncElementList where
  arbitrary = do
    len <- choose (0, 6)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (TableFuncElementList (x :| xs))
