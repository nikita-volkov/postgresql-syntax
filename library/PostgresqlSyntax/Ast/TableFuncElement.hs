module PostgresqlSyntax.Ast.TableFuncElement where

import PostgresqlSyntax.Ast.AnyName
import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.Typename
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

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
  toTextBuilder (TableFuncElement a b c) = toTextBuilder a <> " " <> toTextBuilder b <> suffixMaybe collateClause c
    where
      collateClause a' = "COLLATE " <> toTextBuilder a'
  parser = do
    a <- wrapToHead colId
    space1
    b <- parser
    c <- optional (space1 *> collateClause)
    return (TableFuncElement a b c)
    where
      collateClause = keyword "collate" *> space1 *> endHead *> parser

instance Arbitrary TableFuncElement where
  arbitrary = TableFuncElement <$> arbitrary <*> arbitrary <*> scale (`div` 2) arbitrary
