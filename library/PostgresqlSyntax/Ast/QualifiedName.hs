module PostgresqlSyntax.Ast.QualifiedName where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- columnref:
--   | ColId
--   | ColId indirection
-- qualified_name:
--   | ColId
--   | ColId indirection
-- @
data QualifiedName
  = SimpleQualifiedName Ident
  | IndirectedQualifiedName Ident Indirection
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst QualifiedName where
  toTextBuilder = \case
    SimpleQualifiedName a -> toTextBuilder a
    IndirectedQualifiedName a b -> toTextBuilder a <> toTextBuilder b
  parser =
    IndirectedQualifiedName <$> wrapToHead colId <*> (space *> parser)
      <|> SimpleQualifiedName <$> colId

instance Arbitrary QualifiedName where
  arbitrary =
    oneof
      [ SimpleQualifiedName <$> arbitrary,
        IndirectedQualifiedName <$> arbitrary <*> scale (`div` 2) arbitrary
      ]
