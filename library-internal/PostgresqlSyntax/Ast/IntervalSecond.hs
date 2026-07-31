module PostgresqlSyntax.Ast.IntervalSecond where

import PostgresqlSyntax.Algebra
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder _settings (IntervalSecond a) = case a of
    Nothing -> "SECOND"
    Just a' -> "SECOND " <> TextBuilders.renderInParens (TextBuilder.int64Dec a')
  parser _settings = do
    Parsers.keyword "second"
    a <- optional (Parsers.space *> Parsers.inParens Parsers.decimal)
    return (IntervalSecond a)

instance Qc.Arbitrary IntervalSecond where
  shrink = Qc.genericShrink
  arbitrary = IntervalSecond <$> Qc.oneof [pure Nothing, Just <$> nonNegative]
    where
      nonNegative = Qc.sized (\n -> Qc.choose (0, cap n))
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n
