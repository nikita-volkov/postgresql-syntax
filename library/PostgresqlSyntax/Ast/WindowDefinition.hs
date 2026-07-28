module PostgresqlSyntax.Ast.WindowDefinition where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.WindowSpecification
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
  parser = WindowDefinition <$> (colId <* Parser.space1 <* keyword "as" <* Parser.space1 <* Parser.endHead) <*> parser

instance Qc.Arbitrary WindowDefinition where
  shrink = Qc.genericShrink
  arbitrary = WindowDefinition <$> arbitrary <*> arbitrary
