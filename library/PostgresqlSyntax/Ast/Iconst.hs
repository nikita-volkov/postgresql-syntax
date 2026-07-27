module PostgresqlSyntax.Ast.Iconst where

import PostgresqlSyntax.Extras.HeadedMegaparsec (decimal)
import PostgresqlSyntax.Extras.TextBuilder (int64Dec)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude

-- |
-- ==== References
-- @
-- Iconst
-- @
newtype Iconst = Iconst Int64
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Iconst where
  toTextBuilder (Iconst a) = int64Dec a
  parser = Iconst <$> decimal

instance Arbitrary Iconst where
  arbitrary = Iconst <$> sized (\n -> choose (0, cap n))
    where
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n
