module PostgresqlSyntax.Ast.WindowDefinition where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.WindowSpecification
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
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
  toTextBuilder settings (WindowDefinition a b) = toTextBuilder settings a <> " AS " <> toTextBuilder settings b
  parser settings = WindowDefinition <$> (colId settings <* Parsers.space1 <* Parsers.keyword "as" <* Parsers.space1 <* Parser.endHead) <*> parser settings

instance Qc.Arbitrary WindowDefinition where
  shrink = Qc.genericShrink
  arbitrary = WindowDefinition <$> arbitrary <*> arbitrary
