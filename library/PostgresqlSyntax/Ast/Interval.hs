module PostgresqlSyntax.Ast.Interval where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.IntervalSecond
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

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
  toTextBuilder = \case
    YearInterval -> "YEAR"
    MonthInterval -> "MONTH"
    DayInterval -> "DAY"
    HourInterval -> "HOUR"
    MinuteInterval -> "MINUTE"
    SecondInterval a -> toTextBuilder a
    YearToMonthInterval -> "YEAR TO MONTH"
    DayToHourInterval -> "DAY TO HOUR"
    DayToMinuteInterval -> "DAY TO MINUTE"
    DayToSecondInterval a -> "DAY TO " <> toTextBuilder a
    HourToMinuteInterval -> "HOUR TO MINUTE"
    HourToSecondInterval a -> "HOUR TO " <> toTextBuilder a
    MinuteToSecondInterval a -> "MINUTE TO " <> toTextBuilder a
  parser =
    asum
      [ YearToMonthInterval <$ keyphrase "year to month",
        DayToHourInterval <$ keyphrase "day to hour",
        DayToMinuteInterval <$ keyphrase "day to minute",
        DayToSecondInterval <$> (keyphrase "day to" *> space1 *> endHead *> parser),
        HourToMinuteInterval <$ keyphrase "hour to minute",
        HourToSecondInterval <$> (keyphrase "hour to" *> space1 *> endHead *> parser),
        MinuteToSecondInterval <$> (keyphrase "minute to" *> space1 *> endHead *> parser),
        YearInterval <$ keyword "year",
        MonthInterval <$ keyword "month",
        DayInterval <$ keyword "day",
        HourInterval <$ keyword "hour",
        MinuteInterval <$ keyword "minute",
        SecondInterval <$> parser
      ]

instance Arbitrary Interval where
  arbitrary =
    oneof
      [ pure YearInterval,
        pure MonthInterval,
        pure DayInterval,
        pure HourInterval,
        pure MinuteInterval,
        SecondInterval <$> arbitrary,
        pure YearToMonthInterval,
        pure DayToHourInterval,
        pure DayToMinuteInterval,
        DayToSecondInterval <$> arbitrary,
        pure HourToMinuteInterval,
        HourToSecondInterval <$> arbitrary,
        MinuteToSecondInterval <$> arbitrary
      ]
