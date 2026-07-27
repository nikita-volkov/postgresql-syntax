module PostgresqlSyntax.Ast.ExtractArg where

import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.Sconst
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

-- |
-- ==== References
-- @
-- extract_arg:
--   | IDENT
--   | YEAR_P
--   | MONTH_P
--   | DAY_P
--   | HOUR_P
--   | MINUTE_P
--   | SECOND_P
--   | Sconst
-- @
data ExtractArg
  = IdentExtractArg Ident
  | YearExtractArg
  | MonthExtractArg
  | DayExtractArg
  | HourExtractArg
  | MinuteExtractArg
  | SecondExtractArg
  | SconstExtractArg Sconst
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ExtractArg where
  toTextBuilder = \case
    IdentExtractArg a -> toTextBuilder a
    YearExtractArg -> "YEAR"
    MonthExtractArg -> "MONTH"
    DayExtractArg -> "DAY"
    HourExtractArg -> "HOUR"
    MinuteExtractArg -> "MINUTE"
    SecondExtractArg -> "SECOND"
    SconstExtractArg a -> toTextBuilder a
  parser =
    asum
      [ YearExtractArg <$ keyword "year",
        MonthExtractArg <$ keyword "month",
        DayExtractArg <$ keyword "day",
        HourExtractArg <$ keyword "hour",
        MinuteExtractArg <$ keyword "minute",
        SecondExtractArg <$ keyword "second",
        SconstExtractArg <$> parser,
        IdentExtractArg <$> parser
      ]

instance Arbitrary ExtractArg where
  arbitrary =
    oneof
      [ IdentExtractArg <$> arbitrary,
        pure YearExtractArg,
        pure MonthExtractArg,
        pure DayExtractArg,
        pure HourExtractArg,
        pure MinuteExtractArg,
        pure SecondExtractArg,
        SconstExtractArg <$> arbitrary
      ]
