module PostgresqlSyntax.Ast.ValuesClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.ExprList
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- values_clause:
--   |  VALUES '(' expr_list ')'
--   |  values_clause ',' '(' expr_list ')'
-- @
newtype ValuesClause = ValuesClause (NonEmpty ExprList)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ValuesClause where
  toTextBuilder settings (ValuesClause a) = "VALUES " <> TextBuilders.commaNonEmpty (TextBuilders.renderInParens . toTextBuilder settings) a
  parser settings = do
    Parsers.keyword "values"
    Parsers.space
    ValuesClause <$> Parsers.sep1 Parsers.commaSeparator row
    where
      row = do
        Parsers.char '('
        Parser.endHead
        Parsers.space
        a <- ExprList <$> Parsers.sep1 Parsers.commaSeparator (parser settings)
        Parsers.space
        Parsers.char ')'
        return a

instance Qc.Arbitrary ValuesClause where
  shrink = Qc.genericShrink
  arbitrary = ValuesClause <$> Gens.nonEmptyUpTo 7 Qc.arbitrary
