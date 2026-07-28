module PostgresqlSyntax.Ast.RelationExpr where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.QualifiedName
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
    Parser.label "relation expression" $
      asum
        [ do
            keyword "only"
            Parser.space1
            name <- parser
            return (OnlyRelationExpr name False),
          inParensWithClause (keyword "only") parser <&> \a -> OnlyRelationExpr a True,
          do
            name <- parser
            asterisk <-
              asum
                [ True <$ (Parser.space1 *> Parser.char '*'),
                  pure False
                ]
            return (SimpleRelationExpr name asterisk)
        ]

instance Qc.Arbitrary RelationExpr where
  arbitrary =
    Qc.oneof
      [ SimpleRelationExpr <$> Qc.arbitrary <*> Qc.arbitrary,
        OnlyRelationExpr <$> Qc.arbitrary <*> Qc.arbitrary
      ]
