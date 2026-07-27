module PostgresqlSyntax.Ast.RelationExpr where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.QualifiedName
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

-- |
-- ==== References
-- @
-- | qualified_name
-- | qualified_name '*'
-- | ONLY qualified_name
-- | ONLY '(' qualified_name ')'
-- @
data RelationExpr
  = -- | Name, then whether an asterisk is present.
    SimpleRelationExpr QualifiedName Bool
  | -- | Name, then whether parentheses are present.
    OnlyRelationExpr QualifiedName Bool
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst RelationExpr where
  toTextBuilder = \case
    SimpleRelationExpr a b -> toTextBuilder a <> bool "" " *" b
    OnlyRelationExpr a b -> "ONLY " <> bool toTextBuilder (renderInParens . toTextBuilder) b a
  parser =
    label "relation expression"
      $ asum
        [ do
            keyword "only"
            space1
            name <- parser
            return (OnlyRelationExpr name False),
          inParensWithClause (keyword "only") parser <&> \a -> OnlyRelationExpr a True,
          do
            name <- parser
            asterisk <-
              asum
                [ True <$ (space1 *> char '*'),
                  pure False
                ]
            return (SimpleRelationExpr name asterisk)
        ]

instance Arbitrary RelationExpr where
  arbitrary =
    oneof
      [ SimpleRelationExpr <$> arbitrary <*> arbitrary,
        OnlyRelationExpr <$> arbitrary <*> arbitrary
      ]
