module PostgresqlSyntax.Ast.ConstDatetime where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.Timezone
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
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
  toTextBuilder = \case
    TimestampConstDatetime a b ->
      optLexemes
        [ Just "TIMESTAMP",
          fmap (renderInParens . TextBuilder.int64Dec) a,
          fmap toTextBuilder b
        ]
    TimeConstDatetime a b ->
      optLexemes
        [ Just "TIME",
          fmap (renderInParens . TextBuilder.int64Dec) a,
          fmap toTextBuilder b
        ]
  parser =
    asum
      [ do
          keyword "timestamp"
          a <- optional (Parser.space1 *> inParens Parser.decimal)
          b <- optional (Parser.space1 *> parser)
          return (TimestampConstDatetime a b),
        do
          keyword "time"
          a <- optional (Parser.space1 *> inParens Parser.decimal)
          b <- optional (Parser.space1 *> parser)
          return (TimeConstDatetime a b)
      ]

instance Qc.Arbitrary ConstDatetime where
  -- \| The precision here is parsed via 'Parser.decimal' (unsigned), so it
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
