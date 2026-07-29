module PostgresqlSyntax.Ast.QualifiedName where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
  toTextBuilder settings = \case
    SimpleQualifiedName a -> toTextBuilder settings a
    IndirectedQualifiedName a b -> toTextBuilder settings a <> toTextBuilder settings b
  parser settings =
    IndirectedQualifiedName
      <$> Parser.wrapToHead (colId settings)
      <*> (Parsers.space *> parser settings)
        <|> SimpleQualifiedName
      <$> colId settings

instance Qc.Arbitrary QualifiedName where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ SimpleQualifiedName <$> Qc.arbitrary,
        IndirectedQualifiedName <$> Qc.arbitrary <*> Qc.arbitrary
      ]
