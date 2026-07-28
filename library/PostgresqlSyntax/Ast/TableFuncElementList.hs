module PostgresqlSyntax.Ast.TableFuncElementList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TableFuncElement
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder (TableFuncElementList a) = commaNonEmpty toTextBuilder a
  parser = TableFuncElementList <$> Parser.sep1 commaSeparator parser

instance Qc.Arbitrary TableFuncElementList where
  shrink = Qc.genericShrink
  arbitrary = TableFuncElementList <$> Qc.nonEmptyUpTo 6 Qc.arbitrary
