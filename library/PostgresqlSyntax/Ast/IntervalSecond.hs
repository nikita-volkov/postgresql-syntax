module PostgresqlSyntax.Ast.IntervalSecond where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.Extras.TextBuilder (int64Dec)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, option, some, try)

-- |
-- ==== References
-- @
-- interval_second:
--   | SECOND_P
--   | SECOND_P '(' Iconst ')'
-- @
newtype IntervalSecond = IntervalSecond (Maybe Int64)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst IntervalSecond where
  toTextBuilder (IntervalSecond a) = case a of
    Nothing -> "SECOND"
    Just a' -> "SECOND " <> renderInParens (int64Dec a')
  parser = do
    keyword "second"
    a <- optional (space *> inParens decimal)
    return (IntervalSecond a)

instance Arbitrary IntervalSecond where
  arbitrary = IntervalSecond <$> oneof [pure Nothing, Just <$> nonNegative]
    where
      nonNegative = sized (\n -> choose (0, cap n))
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n
