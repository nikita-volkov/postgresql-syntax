module PostgresqlSyntax.Ast.OnConflict where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.ConfExpr
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.OnConflictDo
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- opt_on_conflict:
--   | ON CONFLICT opt_conf_expr DO UPDATE SET set_clause_list where_clause
--   | ON CONFLICT opt_conf_expr DO NOTHING
--   | EMPTY
-- @
data OnConflict = OnConflict (Maybe ConfExpr) OnConflictDo
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OnConflict where
  toTextBuilder (OnConflict a b) = "ON CONFLICT" <> suffixMaybe toTextBuilder a <> " DO " <> toTextBuilder b
  parser = do
    keyword "on"
    space1
    keyword "conflict"
    space1
    endHead
    a <- optional (parser <* space1)
    keyword "do"
    space1
    b <- parser
    return (OnConflict a b)

instance Arbitrary OnConflict where
  arbitrary = OnConflict <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
