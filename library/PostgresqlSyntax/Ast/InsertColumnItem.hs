module PostgresqlSyntax.Ast.InsertColumnItem where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder (InsertColumnItem a b) = toTextBuilder a <> suffixMaybe toTextBuilder b
  parser = do
    a <- colId
    Parser.endHead
    b <- optional (Parser.space1 *> parser)
    return (InsertColumnItem a b)

instance Qc.Arbitrary InsertColumnItem where
  shrink = Qc.genericShrink
  arbitrary = InsertColumnItem <$> arbitrary <*> Qc.terminatingMaybe arbitrary
