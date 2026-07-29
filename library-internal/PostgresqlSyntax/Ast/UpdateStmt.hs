module PostgresqlSyntax.Ast.UpdateStmt where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.RelationExprOptAlias (RelationExprOptAlias)
import qualified PostgresqlSyntax.Ast.RelationExprOptAlias as RelationExprOptAlias
import PostgresqlSyntax.Ast.SetClauseList
import PostgresqlSyntax.Ast.TableRef
import PostgresqlSyntax.Ast.TargetList
import PostgresqlSyntax.Ast.WhereOrCurrentClause
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
--
-- @from_clause@\/@returning_clause@ are bare aliases to @NonEmpty
-- 'PostgresqlSyntax.Ast.TableRef'@\/'PostgresqlSyntax.Ast.TargetList'.
data UpdateStmt = UpdateStmt (Maybe WithClause) RelationExprOptAlias SetClauseList (Maybe (NonEmpty TableRef)) (Maybe WhereOrCurrentClause) (Maybe TargetList)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst UpdateStmt where
  toTextBuilder settings (UpdateStmt a b c d e f) =
    TextBuilders.prefixMaybe (toTextBuilder settings) a
      <> "UPDATE "
      <> toTextBuilder settings b
      <> " "
      <> "SET "
      <> toTextBuilder settings c
      <> TextBuilders.suffixMaybe fromClause d
      <> TextBuilders.suffixMaybe (toTextBuilder settings) e
      <> TextBuilders.suffixMaybe returningClause f
    where
      fromClause a' = "FROM " <> TextBuilders.commaNonEmpty (toTextBuilder settings) a'
      returningClause = mappend "RETURNING " . toTextBuilder settings
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
    d <- optional (Parsers.space1 *> fromClause)
    e <- optional (Parsers.space1 *> parser settings)
    f <- optional (Parsers.space1 *> returningClause)
    return (UpdateStmt a b c d e f)
    where
      fromClause = Parsers.keyword "from" *> Parser.endHead *> Parsers.space1 *> Parsers.sep1 Parsers.commaSeparator (parser settings)
      returningClause = Parsers.keyword "returning" *> Parsers.space1 *> Parser.endHead *> parser settings

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
