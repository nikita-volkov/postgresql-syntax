module PostgresqlSyntax.Ast.Interval where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.IntervalSecond
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- opt_interval:
--   | YEAR_P
--   | MONTH_P
--   | DAY_P
--   | HOUR_P
--   | MINUTE_P
--   | interval_second
--   | YEAR_P TO MONTH_P
--   | DAY_P TO HOUR_P
--   | DAY_P TO MINUTE_P
--   | DAY_P TO interval_second
--   | HOUR_P TO MINUTE_P
--   | HOUR_P TO interval_second
--   | MINUTE_P TO interval_second
--   | EMPTY
-- @
data Interval
  = YearInterval
  | MonthInterval
  | DayInterval
  | HourInterval
  | MinuteInterval
  | SecondInterval IntervalSecond
  | YearToMonthInterval
  | DayToHourInterval
  | DayToMinuteInterval
  | DayToSecondInterval IntervalSecond
  | HourToMinuteInterval
  | HourToSecondInterval IntervalSecond
  | MinuteToSecondInterval IntervalSecond
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Interval where
  toTextBuilder settings = \case
    YearInterval -> "YEAR"
    MonthInterval -> "MONTH"
    DayInterval -> "DAY"
    HourInterval -> "HOUR"
    MinuteInterval -> "MINUTE"
    SecondInterval a -> toTextBuilder settings a
    YearToMonthInterval -> "YEAR TO MONTH"
    DayToHourInterval -> "DAY TO HOUR"
    DayToMinuteInterval -> "DAY TO MINUTE"
    DayToSecondInterval a -> "DAY TO " <> toTextBuilder settings a
    HourToMinuteInterval -> "HOUR TO MINUTE"
    HourToSecondInterval a -> "HOUR TO " <> toTextBuilder settings a
    MinuteToSecondInterval a -> "MINUTE TO " <> toTextBuilder settings a
  parser settings =
    asum
      [ YearToMonthInterval <$ Parsers.keyphrase "year to month",
        DayToHourInterval <$ Parsers.keyphrase "day to hour",
        DayToMinuteInterval <$ Parsers.keyphrase "day to minute",
        DayToSecondInterval <$> (Parsers.keyphrase "day to" *> Parsers.space1 *> Parser.endHead *> parser settings),
        HourToMinuteInterval <$ Parsers.keyphrase "hour to minute",
        HourToSecondInterval <$> (Parsers.keyphrase "hour to" *> Parsers.space1 *> Parser.endHead *> parser settings),
        MinuteToSecondInterval <$> (Parsers.keyphrase "minute to" *> Parsers.space1 *> Parser.endHead *> parser settings),
        YearInterval <$ Parsers.keyword "year",
        MonthInterval <$ Parsers.keyword "month",
        DayInterval <$ Parsers.keyword "day",
        HourInterval <$ Parsers.keyword "hour",
        MinuteInterval <$ Parsers.keyword "minute",
        SecondInterval <$> parser settings
      ]

instance Qc.Arbitrary Interval where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ pure YearInterval,
        pure MonthInterval,
        pure DayInterval,
        pure HourInterval,
        pure MinuteInterval,
        SecondInterval <$> Qc.arbitrary,
        pure YearToMonthInterval,
        pure DayToHourInterval,
        pure DayToMinuteInterval,
        DayToSecondInterval <$> Qc.arbitrary,
        pure HourToMinuteInterval,
        HourToSecondInterval <$> Qc.arbitrary,
        MinuteToSecondInterval <$> Qc.arbitrary
      ]
