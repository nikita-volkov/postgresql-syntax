module PostgresqlSyntax.Ast.RelationExprOptAlias
  ( RelationExprOptAlias (..),
  )
where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.RelationExpr
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
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
  toTextBuilder settings (RelationExprOptAlias a b) = toTextBuilder settings a <> TextBuilders.suffixMaybe optAlias b
    where
      optAlias (c, d) = bool "" "AS " c <> toTextBuilder settings d
  parser settings = do
    a <- parser settings
    b <- optional $ do
      Parsers.space1
      b <- Parsers.trueIfPresent (Parsers.keyword "as" *> Parsers.space1)
      -- Only the bare-alias (no @AS@) branch has the shift/reduce conflict
      -- that Postgres resolves by excluding @SET@; see @gram.y@, comment
      -- above @relation_expr_opt_alias@'s first production.
      c <- Parsers.filteredColIdLike UnquotedIdent (parser settings) (if b then [] else ["set"])
      return (b, c)
    return (RelationExprOptAlias a b)

instance Qc.Arbitrary RelationExprOptAlias where
  shrink = Qc.genericShrink
  arbitrary = RelationExprOptAlias <$> arbitrary <*> arbitrary
