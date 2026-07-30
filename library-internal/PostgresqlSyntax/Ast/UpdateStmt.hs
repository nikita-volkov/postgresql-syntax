module PostgresqlSyntax.Ast.UpdateStmt where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.FromClause
import PostgresqlSyntax.Ast.RelationExprOptAlias (RelationExprOptAlias)
import qualified PostgresqlSyntax.Ast.RelationExprOptAlias as RelationExprOptAlias
import PostgresqlSyntax.Ast.ReturningClause
import PostgresqlSyntax.Ast.SetClauseList
import PostgresqlSyntax.Ast.WhereOrCurrentClause
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- UpdateStmt:
--   | opt_with_clause UPDATE relation_expr_opt_alias
--       SET set_clause_list
--       from_clause
--       where_or_current_clause
--       returning_clause
-- @
data UpdateStmt = UpdateStmt (Maybe WithClause) RelationExprOptAlias SetClauseList (Maybe FromClause) (Maybe WhereOrCurrentClause) (Maybe ReturningClause)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst UpdateStmt where
  toTextBuilder settings (UpdateStmt a b c d e f) =
    TextBuilders.prefixMaybe (toTextBuilder settings) a
      <> "UPDATE "
      <> toTextBuilder settings b
      <> " "
      <> "SET "
      <> toTextBuilder settings c
      <> TextBuilders.suffixMaybe (toTextBuilder settings) d
      <> TextBuilders.suffixMaybe (toTextBuilder settings) e
      <> TextBuilders.suffixMaybe (toTextBuilder settings) f
  parser settings = do
    a <- optional (Parser.wrapToHead (parser settings) <* Parsers.space1)
    Parsers.keyword "update"
    Parsers.space1
    Parser.endHead
    b <- RelationExprOptAlias.customizedParser settings ["set"]
    Parsers.space1
    Parsers.keyword "set"
    Parsers.space1
    c <- parser settings
    d <- optional (Parsers.space1 *> parser settings)
    e <- optional (Parsers.space1 *> parser settings)
    f <- optional (Parsers.space1 *> parser settings)
    return (UpdateStmt a b c d e f)

instance Qc.Arbitrary UpdateStmt where
  shrink = Qc.genericShrink
  arbitrary =
    UpdateStmt
      <$> Gens.terminatingMaybe (Gens.downscale Qc.arbitrary)
      <*> Qc.arbitrary
      <*> Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
