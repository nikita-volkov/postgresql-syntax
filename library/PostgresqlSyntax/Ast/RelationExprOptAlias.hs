module PostgresqlSyntax.Ast.RelationExprOptAlias
  ( RelationExprOptAlias (..),
    customizedParser,
  )
where

import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.RelationExpr
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

-- |
-- ==== References
-- @
-- relation_expr_opt_alias:
--   | relation_expr
--   | relation_expr ColId
--   | relation_expr AS ColId
-- @
data RelationExprOptAlias = RelationExprOptAlias RelationExpr (Maybe (Bool, Ident))
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst RelationExprOptAlias where
  toTextBuilder (RelationExprOptAlias a b) = toTextBuilder a <> suffixMaybe optAlias b
    where
      optAlias (c, d) = bool "" "AS " c <> toTextBuilder d
  parser = customizedParser []

-- |
-- Parameterized over the alias identifier's excluded reserved words —
-- callers like @update_stmt@\/@delete_stmt@ need to keep e.g. @SET@\/
-- @USING@\/@WHERE@\/@RETURNING@ from being swallowed as a bare (unaliased)
-- alias. Mirrors the pre-extraction @relationExprOptAlias@ taking a
-- @reservedKeywords@ argument.
customizedParser :: [Text] -> Parser RelationExprOptAlias
customizedParser reservedKeywords = do
  a <- parser
  b <- optional $ do
    space1
    b <- trueIfPresent (keyword "as" *> space1)
    c <- filteredColIdLike UnquotedIdent parser reservedKeywords
    return (b, c)
  return (RelationExprOptAlias a b)

instance Arbitrary RelationExprOptAlias where
  arbitrary = RelationExprOptAlias <$> arbitrary <*> arbitrary
