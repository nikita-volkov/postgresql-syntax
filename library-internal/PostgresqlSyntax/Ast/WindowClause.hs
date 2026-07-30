module PostgresqlSyntax.Ast.WindowClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.WindowDefinition
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- window_clause:
--   |  WINDOW window_definition_list
--   |  /*EMPTY*/
-- @
newtype WindowClause = WindowClause (NonEmpty WindowDefinition)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst WindowClause where
  toTextBuilder settings (WindowClause a) = "WINDOW " <> TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = do
    Parsers.keyword "window"
    Parser.endHead
    Parsers.space1
    WindowClause <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary WindowClause where
  shrink = Qc.genericShrink
  arbitrary = WindowClause <$> Gens.nonEmptyUpTo 6 Qc.arbitrary
