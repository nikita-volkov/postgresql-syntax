module PostgresqlSyntax.Ast.TablesampleClause where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.FuncName
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- tablesample_clause:
--   | TABLESAMPLE func_name '(' expr_list ')' opt_repeatable_clause
-- @
--
-- @opt_repeatable_clause@ is a bare alias to 'PostgresqlSyntax.Ast.AExpr'.
data TablesampleClause = TablesampleClause FuncName ExprList (Maybe AExpr)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TablesampleClause where
  toTextBuilder settings (TablesampleClause a b c) =
    "TABLESAMPLE " <> toTextBuilder settings a <> " (" <> toTextBuilder settings b <> ")" <> TextBuilders.suffixMaybe repeatableClause c
    where
      repeatableClause a' = "REPEATABLE (" <> toTextBuilder settings a' <> ")"
  parser settings = do
    Parsers.keyword "tablesample"
    Parsers.space1
    Parser.endHead
    a <- parser settings
    Parsers.space
    b <- Parsers.inParens (parser settings)
    c <- optional (Parsers.space *> repeatableClause)
    return (TablesampleClause a b c)
    where
      repeatableClause = do
        Parsers.keyword "repeatable"
        Parsers.space
        Parsers.inParens (Parser.endHead *> parser settings)

instance Qc.Arbitrary TablesampleClause where
  shrink = Qc.genericShrink
  arbitrary = TablesampleClause <$> arbitrary <*> arbitrary <*> Gens.terminatingMaybe (Gens.downscale arbitrary)
