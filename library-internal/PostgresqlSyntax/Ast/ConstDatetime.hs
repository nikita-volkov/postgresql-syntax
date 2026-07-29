module PostgresqlSyntax.Ast.ConstDatetime where

import PostgresqlSyntax.Ast.Timezone
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- ConstDatetime:
--   | TIMESTAMP '(' Iconst ')' opt_timezone
--   | TIMESTAMP opt_timezone
--   | TIME '(' Iconst ')' opt_timezone
--   | TIME opt_timezone
-- @
data ConstDatetime
  = TimestampConstDatetime (Maybe Int64) (Maybe Timezone)
  | TimeConstDatetime (Maybe Int64) (Maybe Timezone)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ConstDatetime where
  toTextBuilder settings = \case
    TimestampConstDatetime a b ->
      TextBuilders.optLexemes
        [ Just "TIMESTAMP",
          fmap (TextBuilders.renderInParens . TextBuilder.int64Dec) a,
          fmap (toTextBuilder settings) b
        ]
    TimeConstDatetime a b ->
      TextBuilders.optLexemes
        [ Just "TIME",
          fmap (TextBuilders.renderInParens . TextBuilder.int64Dec) a,
          fmap (toTextBuilder settings) b
        ]
  parser settings =
    asum
      [ do
          Parsers.keyword "timestamp"
          a <- optional (Parsers.space1 *> Parsers.inParens Parsers.decimal)
          b <- optional (Parsers.space1 *> parser settings)
          return (TimestampConstDatetime a b),
        do
          Parsers.keyword "time"
          a <- optional (Parsers.space1 *> Parsers.inParens Parsers.decimal)
          b <- optional (Parsers.space1 *> parser settings)
          return (TimeConstDatetime a b)
      ]

instance Qc.Arbitrary ConstDatetime where
  shrink = Qc.genericShrink

  -- The precision here is parsed via 'Parsers.decimal' (unsigned), so it
  -- must never be negative — mirroring
  -- 'PostgresqlSyntax.Ast.IntervalSecond'\'s own @nonNegative@.
  arbitrary =
    Qc.oneof
      [ TimestampConstDatetime <$> nonNegativeMaybeInt64 <*> Qc.arbitrary,
        TimeConstDatetime <$> nonNegativeMaybeInt64 <*> Qc.arbitrary
      ]
    where
      nonNegativeMaybeInt64 = Qc.oneof [pure Nothing, Just <$> nonNegativeInt64]
      nonNegativeInt64 = Qc.sized (\n -> Qc.choose (0, cap n))
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n
