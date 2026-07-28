module PostgresqlSyntax.Ast.UpdateStmt where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.RelationExprOptAlias (RelationExprOptAlias)
import qualified PostgresqlSyntax.Ast.RelationExprOptAlias as RelationExprOptAlias
import PostgresqlSyntax.Ast.SetClauseList
import PostgresqlSyntax.Ast.TableRef
import PostgresqlSyntax.Ast.TargetList
import PostgresqlSyntax.Ast.WhereOrCurrentClause
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder (UpdateStmt a b c d e f) =
    TextBuilders.prefixMaybe toTextBuilder a
      <> "UPDATE "
      <> toTextBuilder b
      <> " "
      <> "SET "
      <> toTextBuilder c
      <> TextBuilders.suffixMaybe fromClause d
      <> TextBuilders.suffixMaybe toTextBuilder e
      <> TextBuilders.suffixMaybe returningClause f
    where
      fromClause a' = "FROM " <> TextBuilders.commaNonEmpty toTextBuilder a'
      returningClause = mappend "RETURNING " . toTextBuilder
  parser = do
    a <- optional (Parser.wrapToHead parser <* Parsers.space1)
    Parsers.keyword "update"
    Parsers.space1
    Parser.endHead
    b <- RelationExprOptAlias.customizedParser ["set"]
    Parsers.space1
    Parsers.keyword "set"
    Parsers.space1
    c <- parser
    d <- optional (Parsers.space1 *> fromClause)
    e <- optional (Parsers.space1 *> parser)
    f <- optional (Parsers.space1 *> returningClause)
    return (UpdateStmt a b c d e f)
    where
      fromClause = Parsers.keyword "from" *> Parser.endHead *> Parsers.space1 *> Parsers.sep1 Parsers.commaSeparator parser
      returningClause = Parsers.keyword "returning" *> Parsers.space1 *> Parser.endHead *> parser

instance Qc.Arbitrary UpdateStmt where
  shrink = Qc.genericShrink
  arbitrary =
    UpdateStmt
      <$> Qc.terminatingMaybe (Qc.downscale Qc.arbitrary)
      <*> Qc.arbitrary
      <*> Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary
