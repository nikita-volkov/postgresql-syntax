module PostgresqlSyntax.Ast.IntervalSecond where

import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder (IntervalSecond a) = case a of
    Nothing -> "SECOND"
    Just a' -> "SECOND " <> renderInParens (TextBuilder.int64Dec a')
  parser = do
    keyword "second"
    a <- optional (Parser.space *> inParens Parser.decimal)
    return (IntervalSecond a)

instance Qc.Arbitrary IntervalSecond where
  shrink = Qc.genericShrink
  arbitrary = IntervalSecond <$> Qc.oneof [pure Nothing, Just <$> nonNegative]
    where
      nonNegative = Qc.sized (\n -> Qc.choose (0, cap n))
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n
