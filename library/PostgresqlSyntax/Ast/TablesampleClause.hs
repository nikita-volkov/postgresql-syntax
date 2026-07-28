module PostgresqlSyntax.Ast.TablesampleClause where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.FuncName
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder (TablesampleClause a b c) =
    "TABLESAMPLE " <> toTextBuilder a <> " (" <> toTextBuilder b <> ")" <> TextBuilders.suffixMaybe repeatableClause c
    where
      repeatableClause a' = "REPEATABLE (" <> toTextBuilder a' <> ")"
  parser = do
    Parsers.keyword "tablesample"
    Parsers.space1
    Parser.endHead
    a <- parser
    Parsers.space
    b <- Parsers.inParens parser
    c <- optional (Parsers.space *> repeatableClause)
    return (TablesampleClause a b c)
    where
      repeatableClause = do
        Parsers.keyword "repeatable"
        Parsers.space
        Parsers.inParens (Parser.endHead *> parser)

instance Qc.Arbitrary TablesampleClause where
  shrink = Qc.genericShrink
  arbitrary = TablesampleClause <$> arbitrary <*> arbitrary <*> Qc.terminatingMaybe (Qc.downscale arbitrary)
