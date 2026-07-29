module PostgresqlSyntax.Ast.InsertTarget where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.QualifiedName
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder (InsertTarget a b) = toTextBuilder a <> foldMap (mappend " AS " . toTextBuilder) b
  parser = do
    a <- parser
    Parser.endHead
    b <- optional (Parsers.space1 *> Parsers.keyword "as" *> Parsers.space1 *> Parser.endHead *> colId)
    return (InsertTarget a b)

instance Qc.Arbitrary InsertTarget where
  shrink = Qc.genericShrink
  arbitrary = InsertTarget <$> arbitrary <*> arbitrary
