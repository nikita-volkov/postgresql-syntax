module PostgresqlSyntax.Ast.MathOp where

import HeadedMegaparsec hiding (string)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.Extras.TextBuilder (char7)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (many, option, some, try)

-- |
-- ==== References
-- @
-- MathOp:
--   | '+'
--   | '-'
--   | '*'
--   | '/'
--   | '%'
--   | '^'
--   | '<'
--   | '>'
--   | '='
--   | LESS_EQUALS
--   | GREATER_EQUALS
--   | NOT_EQUALS
-- @
data MathOp
  = PlusMathOp
  | MinusMathOp
  | AsteriskMathOp
  | SlashMathOp
  | PercentMathOp
  | ArrowUpMathOp
  | ArrowLeftMathOp
  | ArrowRightMathOp
  | EqualsMathOp
  | LessEqualsMathOp
  | GreaterEqualsMathOp
  | ArrowLeftArrowRightMathOp
  | ExclamationEqualsMathOp
  deriving (Show, Generic, Eq, Ord, Data, Enum, Bounded)

instance IsAst MathOp where
  toTextBuilder = \case
    PlusMathOp -> char7 '+'
    MinusMathOp -> char7 '-'
    AsteriskMathOp -> char7 '*'
    SlashMathOp -> char7 '/'
    PercentMathOp -> char7 '%'
    ArrowUpMathOp -> char7 '^'
    ArrowLeftMathOp -> char7 '<'
    ArrowRightMathOp -> char7 '>'
    EqualsMathOp -> char7 '='
    LessEqualsMathOp -> "<="
    GreaterEqualsMathOp -> ">="
    ArrowLeftArrowRightMathOp -> "<>"
    ExclamationEqualsMathOp -> "!="
  parser =
    asum
      [ ArrowLeftArrowRightMathOp <$ string' "<>",
        GreaterEqualsMathOp <$ string' ">=",
        ExclamationEqualsMathOp <$ string' "!=",
        LessEqualsMathOp <$ string' "<=",
        PlusMathOp <$ char '+',
        MinusMathOp <$ char '-',
        AsteriskMathOp <$ char '*',
        SlashMathOp <$ char '/',
        PercentMathOp <$ char '%',
        ArrowUpMathOp <$ char '^',
        ArrowLeftMathOp <$ char '<',
        ArrowRightMathOp <$ char '>',
        EqualsMathOp <$ char '='
      ]

instance Arbitrary MathOp where
  arbitrary = elements [minBound .. maxBound]
