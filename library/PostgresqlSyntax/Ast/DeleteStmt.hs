module PostgresqlSyntax.Ast.DeleteStmt where

import qualified PostgresqlSyntax.Ast.RelationExprOptAlias as RelationExprOptAlias
import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.RelationExprOptAlias (RelationExprOptAlias)
import PostgresqlSyntax.Ast.TableRef
import PostgresqlSyntax.Ast.TargetList
import PostgresqlSyntax.Ast.WhereOrCurrentClause
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- DeleteStmt:
--   | opt_with_clause DELETE_P FROM relation_expr_opt_alias
--       using_clause where_or_current_clause returning_clause
-- @
--
-- @using_clause@\/@returning_clause@ are bare aliases to @NonEmpty
-- 'PostgresqlSyntax.Ast.TableRef'@\/'PostgresqlSyntax.Ast.TargetList'.
data DeleteStmt = DeleteStmt (Maybe WithClause) RelationExprOptAlias (Maybe (NonEmpty TableRef)) (Maybe WhereOrCurrentClause) (Maybe TargetList)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst DeleteStmt where
  toTextBuilder (DeleteStmt a b c d e) =
    prefixMaybe toTextBuilder a
      <> "DELETE FROM "
      <> toTextBuilder b
      <> suffixMaybe usingClause c
      <> suffixMaybe toTextBuilder d
      <> suffixMaybe returningClause e
    where
      usingClause a' = "USING " <> commaNonEmpty toTextBuilder a'
      returningClause = mappend "RETURNING " . toTextBuilder
  parser = do
    a <- optional (Parser.wrapToHead parser <* Parser.space1)
    keyword "delete"
    Parser.space1
    Parser.endHead
    keyword "from"
    Parser.space1
    b <- RelationExprOptAlias.customizedParser ["using", "where", "returning"]
    c <- optional (Parser.space1 *> usingClause)
    d <- optional (Parser.space1 *> parser)
    e <- optional (Parser.space1 *> returningClause)
    return (DeleteStmt a b c d e)
    where
      usingClause = keyword "using" *> Parser.space1 *> Parser.sep1 commaSeparator parser
      returningClause = keyword "returning" *> Parser.space1 *> Parser.endHead *> parser

instance Qc.Arbitrary DeleteStmt where
  arbitrary =
    DeleteStmt
      <$> Qc.scale (`div` 6) Qc.arbitrary
      <*> Qc.arbitrary
      <*> Qc.scale (`div` 4) Qc.arbitrary
      <*> Qc.scale (`div` 4) Qc.arbitrary
      <*> Qc.scale (`div` 4) Qc.arbitrary
