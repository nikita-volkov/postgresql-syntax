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
  arbitrary =
    Qc.oneof
      [ TimestampConstDatetime <$> Qc.arbitrary <*> Qc.arbitrary,
        TimeConstDatetime <$> Qc.arbitrary <*> Qc.arbitrary
      ]
