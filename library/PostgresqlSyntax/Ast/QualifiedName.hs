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
  toTextBuilder = \case
    SimpleQualifiedName a -> toTextBuilder a
    IndirectedQualifiedName a b -> toTextBuilder a <> toTextBuilder b
  parser =
    IndirectedQualifiedName
      <$> Parser.wrapToHead colId
      <*> (Parsers.space *> parser)
        <|> SimpleQualifiedName
      <$> colId

instance Qc.Arbitrary QualifiedName where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ SimpleQualifiedName <$> Qc.arbitrary,
        IndirectedQualifiedName <$> Qc.arbitrary <*> Qc.arbitrary
      ]
