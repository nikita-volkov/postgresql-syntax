module PostgresqlSyntax.Ast.RelationExprOptAlias
  ( RelationExprOptAlias (..),
    customizedParser,
  )
where

import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.RelationExpr
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
  toTextBuilder (RelationExprOptAlias a b) = toTextBuilder a <> TextBuilders.suffixMaybe optAlias b
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
    Parsers.space1
    b <- Parsers.trueIfPresent (Parsers.keyword "as" *> Parsers.space1)
    c <- Parsers.filteredColIdLike UnquotedIdent parser reservedKeywords
    return (b, c)
  return (RelationExprOptAlias a b)

instance Qc.Arbitrary RelationExprOptAlias where
  shrink = Qc.genericShrink
  arbitrary = RelationExprOptAlias <$> arbitrary <*> arbitrary
