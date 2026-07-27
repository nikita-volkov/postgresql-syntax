module PostgresqlSyntax.Ast.SetTarget where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- set_target:
--   | ColId opt_indirection
-- @
data SetTarget = SetTarget Ident (Maybe Indirection)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SetTarget where
  toTextBuilder (SetTarget a b) = toTextBuilder a <> suffixMaybe toTextBuilder b
  parser = do
    a <- colId
    endHead
    b <- optional (space1 *> parser)
    return (SetTarget a b)

instance Arbitrary SetTarget where
  arbitrary = SetTarget <$> arbitrary <*> scale (`div` 2) arbitrary
