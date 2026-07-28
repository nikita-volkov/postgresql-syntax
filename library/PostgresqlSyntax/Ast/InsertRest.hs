module PostgresqlSyntax.Ast.InsertRest where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.InsertColumnList
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.OverrideKind
import PostgresqlSyntax.Ast.SelectStmt
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- insert_rest:
--   | SelectStmt
--   | OVERRIDING override_kind VALUE_P SelectStmt
--   | '(' insert_column_list ')' SelectStmt
--   | '(' insert_column_list ')' OVERRIDING override_kind VALUE_P SelectStmt
--   | DEFAULT VALUES
-- @
data InsertRest
  = SelectInsertRest (Maybe InsertColumnList) (Maybe OverrideKind) SelectStmt
  | DefaultValuesInsertRest
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InsertRest where
  toTextBuilder = \case
    SelectInsertRest a b c ->
      optLexemes
        [ fmap (renderInParens . toTextBuilder) a,
          fmap insertRestOverriding b,
          Just (toTextBuilder c)
        ]
    DefaultValuesInsertRest -> "DEFAULT VALUES"
    where
      insertRestOverriding a = "OVERRIDING " <> toTextBuilder a <> " VALUE"
  parser =
    asum
      [ DefaultValuesInsertRest <$ (keyword "default" *> Parser.space1 *> Parser.endHead *> keyword "values"),
        do
          a <- optional (inParens parser <* Parser.space1)
          b <- optional $ do
            keyword "overriding"
            Parser.space1
            Parser.endHead
            b <- parser
            Parser.space1
            keyword "value"
            Parser.space1
            return b
          c <- parser
          return (SelectInsertRest a b c)
      ]

instance Qc.Arbitrary InsertRest where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ SelectInsertRest <$> Qc.terminatingMaybe Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary,
        pure DefaultValuesInsertRest
      ]
