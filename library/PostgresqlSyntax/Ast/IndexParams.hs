module PostgresqlSyntax.Ast.IndexParams where

import PostgresqlSyntax.Ast.IndexElem
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  arbitrary = do
    len <- Qc.choose (0, 4)
    x <- Qc.scale (`div` 2) Qc.arbitrary
    xs <- Qc.vectorOf len (Qc.scale (`div` 2) Qc.arbitrary)
    pure (IndexParams (x :| xs))
