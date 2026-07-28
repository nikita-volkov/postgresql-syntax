module PostgresqlSyntax.Ast.IndexParams where

import PostgresqlSyntax.Ast.IndexElem
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder (IndexParams a) = commaNonEmpty toTextBuilder a
  parser = IndexParams <$> Parser.sep1 commaSeparator parser

instance Qc.Arbitrary IndexParams where
  shrink = Qc.genericShrink
  arbitrary = IndexParams <$> Qc.nonEmptyUpTo 4 Qc.arbitrary
