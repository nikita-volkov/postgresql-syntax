module PostgresqlSyntax.Ast.Columnref where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident (Ident, colId)
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
-- @
data Columnref = Columnref Ident (Maybe Indirection)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Columnref where
  toTextBuilder (Columnref a b) = toTextBuilder a <> foldMap toTextBuilder b
  parser = do
    a <- wrapToHead colId
    endHead
    b <- optional (space *> parser)
    return (Columnref a b)

instance Arbitrary Columnref where
  arbitrary = Columnref <$> arbitrary <*> scale (`div` 2) arbitrary
