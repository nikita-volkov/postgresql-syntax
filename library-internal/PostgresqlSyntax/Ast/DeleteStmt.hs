module PostgresqlSyntax.Ast.DeleteStmt where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.RelationExprOptAlias (RelationExprOptAlias)
import qualified PostgresqlSyntax.Ast.RelationExprOptAlias as RelationExprOptAlias
import PostgresqlSyntax.Ast.ReturningClause
import PostgresqlSyntax.Ast.UsingClause
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
-- DeleteStmt:
--   | opt_with_clause DELETE_P FROM relation_expr_opt_alias
--       using_clause where_or_current_clause returning_clause
-- @
data DeleteStmt = DeleteStmt (Maybe WithClause) RelationExprOptAlias (Maybe UsingClause) (Maybe WhereOrCurrentClause) (Maybe ReturningClause)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst DeleteStmt where
  toTextBuilder settings (DeleteStmt a b c d e) =
    TextBuilders.prefixMaybe (toTextBuilder settings) a
      <> "DELETE FROM "
      <> toTextBuilder settings b
      <> TextBuilders.suffixMaybe (toTextBuilder settings) c
      <> TextBuilders.suffixMaybe (toTextBuilder settings) d
      <> TextBuilders.suffixMaybe (toTextBuilder settings) e
  parser settings = do
    a <- optional (Parser.wrapToHead (parser settings) <* Parsers.space1)
    Parsers.keyword "delete"
    Parsers.space1
    Parser.endHead
    Parsers.keyword "from"
    Parsers.space1
    b <- RelationExprOptAlias.customizedParser settings ["using", "where", "returning"]
    c <- optional (Parsers.space1 *> parser settings)
    d <- optional (Parsers.space1 *> parser settings)
    e <- optional (Parsers.space1 *> parser settings)
    return (DeleteStmt a b c d e)

instance Qc.Arbitrary DeleteStmt where
  shrink = Qc.genericShrink
  arbitrary =
    DeleteStmt
      <$> Gens.terminatingMaybe (Gens.downscale Qc.arbitrary)
      <*> Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
