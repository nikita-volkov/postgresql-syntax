module PostgresqlSyntax.Ast.DeleteStmt where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.RelationExprOptAlias (RelationExprOptAlias)
import qualified PostgresqlSyntax.Ast.RelationExprOptAlias as RelationExprOptAlias
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
    TextBuilders.prefixMaybe toTextBuilder a
      <> "DELETE FROM "
      <> toTextBuilder b
      <> TextBuilders.suffixMaybe usingClause c
      <> TextBuilders.suffixMaybe toTextBuilder d
      <> TextBuilders.suffixMaybe returningClause e
    where
      usingClause a' = "USING " <> TextBuilders.commaNonEmpty toTextBuilder a'
      returningClause = mappend "RETURNING " . toTextBuilder
  parser = do
    a <- optional (Parser.wrapToHead parser <* Parsers.space1)
    Parsers.keyword "delete"
    Parsers.space1
    Parser.endHead
    Parsers.keyword "from"
    Parsers.space1
    b <- RelationExprOptAlias.customizedParser ["using", "where", "returning"]
    c <- optional (Parsers.space1 *> usingClause)
    d <- optional (Parsers.space1 *> parser)
    e <- optional (Parsers.space1 *> returningClause)
    return (DeleteStmt a b c d e)
    where
      usingClause = Parsers.keyword "using" *> Parsers.space1 *> Parsers.sep1 Parsers.commaSeparator parser
      returningClause = Parsers.keyword "returning" *> Parsers.space1 *> Parser.endHead *> parser

instance Qc.Arbitrary DeleteStmt where
  shrink = Qc.genericShrink
  arbitrary =
    DeleteStmt
      <$> Qc.terminatingMaybe (Qc.downscale Qc.arbitrary)
      <*> Qc.arbitrary
      <*> Qc.arbitrary
      <*> Qc.arbitrary
      <*> Qc.arbitrary
