module PostgresqlSyntax.Ast.MathOp where

import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (many, some, try)
import qualified Test.QuickCheck as Qc

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
    PlusMathOp -> TextBuilder.char7 '+'
    MinusMathOp -> TextBuilder.char7 '-'
    AsteriskMathOp -> TextBuilder.char7 '*'
    SlashMathOp -> TextBuilder.char7 '/'
    PercentMathOp -> TextBuilder.char7 '%'
    ArrowUpMathOp -> TextBuilder.char7 '^'
    ArrowLeftMathOp -> TextBuilder.char7 '<'
    ArrowRightMathOp -> TextBuilder.char7 '>'
    EqualsMathOp -> TextBuilder.char7 '='
    LessEqualsMathOp -> "<="
    GreaterEqualsMathOp -> ">="
    ArrowLeftArrowRightMathOp -> "<>"
    ExclamationEqualsMathOp -> "!="
  parser =
    asum
      [ ArrowLeftArrowRightMathOp <$ Parsers.string' "<>",
        GreaterEqualsMathOp <$ Parsers.string' ">=",
        ExclamationEqualsMathOp <$ Parsers.string' "!=",
        LessEqualsMathOp <$ Parsers.string' "<=",
        PlusMathOp <$ Parsers.char '+',
        MinusMathOp <$ Parsers.char '-',
        AsteriskMathOp <$ Parsers.char '*',
        SlashMathOp <$ Parsers.char '/',
        PercentMathOp <$ Parsers.char '%',
        ArrowUpMathOp <$ Parsers.char '^',
        ArrowLeftMathOp <$ Parsers.char '<',
        ArrowRightMathOp <$ Parsers.char '>',
        EqualsMathOp <$ Parsers.char '='
      ]

instance Qc.Arbitrary MathOp where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [minBound .. maxBound]
