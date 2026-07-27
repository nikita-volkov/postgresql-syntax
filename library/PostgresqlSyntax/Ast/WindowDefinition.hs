module PostgresqlSyntax.Ast.WindowDefinition where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.WindowSpecification
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- window_definition:
--   |  ColId AS window_specification
-- @
data WindowDefinition = WindowDefinition Ident WindowSpecification
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst WindowDefinition where
  toTextBuilder (WindowDefinition a b) = toTextBuilder a <> " AS " <> toTextBuilder b
  parser = WindowDefinition <$> (colId <* space1 <* keyword "as" <* space1 <* endHead) <*> parser

instance Arbitrary WindowDefinition where
  arbitrary = WindowDefinition <$> arbitrary <*> scale (`div` 2) arbitrary
