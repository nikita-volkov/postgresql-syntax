module PostgresqlSyntax.Ast.RelationExpr where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.QualifiedName
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
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
  toTextBuilder settings = \case
    SimpleRelationExpr a b -> toTextBuilder settings a <> bool "" " *" b
    OnlyRelationExpr a b -> "ONLY " <> bool (toTextBuilder settings) (TextBuilders.renderInParens . toTextBuilder settings) b a
  parser settings =
    Parser.label "relation expression" $
      asum
        [ do
            Parsers.keyword "only"
            Parsers.space1
            name <- parser settings
            return (OnlyRelationExpr name False),
          Parsers.inParensWithClause (Parsers.keyword "only") (parser settings) <&> \a -> OnlyRelationExpr a True,
          do
            name <- parser settings
            asterisk <-
              asum
                [ True <$ (Parsers.space1 *> Parsers.char '*'),
                  pure False
                ]
            return (SimpleRelationExpr name asterisk)
        ]

instance Qc.Arbitrary RelationExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ SimpleRelationExpr <$> Qc.arbitrary <*> Qc.arbitrary,
        OnlyRelationExpr <$> Qc.arbitrary <*> Qc.arbitrary
      ]
