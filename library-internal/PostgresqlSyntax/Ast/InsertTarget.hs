module PostgresqlSyntax.Ast.InsertTarget where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.QualifiedName
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- insert_target:
--   | qualified_name
--   | qualified_name AS ColId
-- @
data InsertTarget = InsertTarget QualifiedName (Maybe Ident)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InsertTarget where
  toTextBuilder settings (InsertTarget a b) = toTextBuilder settings a <> foldMap (mappend " AS " . toTextBuilder settings) b
  parser settings = do
    a <- parser settings
    Parser.endHead
    b <- optional (Parsers.space1 *> Parsers.keyword "as" *> Parsers.space1 *> Parser.endHead *> colId settings)
    return (InsertTarget a b)

instance Qc.Arbitrary InsertTarget where
  shrink = Qc.genericShrink
  arbitrary = InsertTarget <$> arbitrary <*> arbitrary
