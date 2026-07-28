module PostgresqlSyntax.Ast.TableFuncElement where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AnyName
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Typename
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- TableFuncElement:
--   | ColId Typename opt_collate_clause
-- @
--
-- @opt_collate_clause@ is a bare alias to 'PostgresqlSyntax.Ast.AnyName'.
data TableFuncElement = TableFuncElement Ident Typename (Maybe AnyName)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TableFuncElement where
  toTextBuilder (TableFuncElement a b c) = toTextBuilder a <> " " <> toTextBuilder b <> TextBuilders.suffixMaybe collateClause c
    where
      collateClause a' = "COLLATE " <> toTextBuilder a'
  parser = do
    a <- Parser.wrapToHead colId
    Parsers.space1
    b <- parser
    c <- optional (Parsers.space1 *> collateClause)
    return (TableFuncElement a b c)
    where
      collateClause = Parsers.keyword "collate" *> Parsers.space1 *> Parser.endHead *> parser

instance Qc.Arbitrary TableFuncElement where
  shrink = Qc.genericShrink
  arbitrary = TableFuncElement <$> arbitrary <*> arbitrary <*> Qc.terminatingMaybe arbitrary
