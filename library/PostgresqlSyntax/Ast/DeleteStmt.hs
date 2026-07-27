module PostgresqlSyntax.Ast.DeleteStmt where

import qualified PostgresqlSyntax.Ast.RelationExprOptAlias as RelationExprOptAlias
import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.RelationExprOptAlias (RelationExprOptAlias)
import PostgresqlSyntax.Ast.TableRef
import PostgresqlSyntax.Ast.TargetList
import PostgresqlSyntax.Ast.WhereOrCurrentClause
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

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
    a <- optional (wrapToHead parser <* space1)
    keyword "delete"
    space1
    endHead
    keyword "from"
    space1
    b <- RelationExprOptAlias.customizedParser ["using", "where", "returning"]
    c <- optional (space1 *> usingClause)
    d <- optional (space1 *> parser)
    e <- optional (space1 *> returningClause)
    return (DeleteStmt a b c d e)
    where
      usingClause = keyword "using" *> space1 *> sep1 commaSeparator parser
      returningClause = keyword "returning" *> space1 *> endHead *> parser

instance Arbitrary DeleteStmt where
  arbitrary =
    DeleteStmt
      <$> scale (`div` 6) arbitrary
      <*> arbitrary
      <*> scale (`div` 4) arbitrary
      <*> scale (`div` 4) arbitrary
      <*> scale (`div` 4) arbitrary
