module PostgresqlSyntax.Ast.ExtractArg where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Sconst
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder settings = \case
    IdentExtractArg a -> toTextBuilder settings a
    YearExtractArg -> "YEAR"
    MonthExtractArg -> "MONTH"
    DayExtractArg -> "DAY"
    HourExtractArg -> "HOUR"
    MinuteExtractArg -> "MINUTE"
    SecondExtractArg -> "SECOND"
    SconstExtractArg a -> toTextBuilder settings a
  parser settings =
    asum
      [ YearExtractArg <$ Parsers.keyword "year",
        MonthExtractArg <$ Parsers.keyword "month",
        DayExtractArg <$ Parsers.keyword "day",
        HourExtractArg <$ Parsers.keyword "hour",
        MinuteExtractArg <$ Parsers.keyword "minute",
        SecondExtractArg <$ Parsers.keyword "second",
        SconstExtractArg <$> parser settings,
        IdentExtractArg <$> parser settings
      ]

instance Qc.Arbitrary ExtractArg where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ IdentExtractArg <$> Qc.arbitrary,
        pure YearExtractArg,
        pure MonthExtractArg,
        pure DayExtractArg,
        pure HourExtractArg,
        pure MinuteExtractArg,
        pure SecondExtractArg,
        SconstExtractArg <$> Qc.arbitrary
      ]
