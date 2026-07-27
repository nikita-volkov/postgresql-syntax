module PostgresqlSyntax.Ast.ConstDatetime where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.Timezone
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.Extras.TextBuilder (int64Dec)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

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
          fmap (renderInParens . int64Dec) a,
          fmap toTextBuilder b
        ]
    TimeConstDatetime a b ->
      optLexemes
        [ Just "TIME",
          fmap (renderInParens . int64Dec) a,
          fmap toTextBuilder b
        ]
  parser =
    asum
      [ do
          keyword "timestamp"
          a <- optional (space1 *> inParens decimal)
          b <- optional (space1 *> parser)
          return (TimestampConstDatetime a b),
        do
          keyword "time"
          a <- optional (space1 *> inParens decimal)
          b <- optional (space1 *> parser)
          return (TimeConstDatetime a b)
      ]

instance Arbitrary ConstDatetime where
  arbitrary =
    oneof
      [ TimestampConstDatetime <$> arbitrary <*> arbitrary,
        TimeConstDatetime <$> arbitrary <*> arbitrary
      ]
