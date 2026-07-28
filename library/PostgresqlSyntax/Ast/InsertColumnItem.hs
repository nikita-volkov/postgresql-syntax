module PostgresqlSyntax.Ast.InsertColumnItem where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- insert_column_item:
--   | ColId opt_indirection
-- @
data InsertColumnItem = InsertColumnItem Ident (Maybe Indirection)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InsertColumnItem where
  toTextBuilder (InsertColumnItem a b) = toTextBuilder a <> TextBuilders.suffixMaybe toTextBuilder b
  parser = do
    a <- colId
    Parser.endHead
    b <- optional (Parsers.space1 *> parser)
    return (InsertColumnItem a b)

instance Qc.Arbitrary InsertColumnItem where
  shrink = Qc.genericShrink
  arbitrary = InsertColumnItem <$> arbitrary <*> Gens.terminatingMaybe arbitrary
