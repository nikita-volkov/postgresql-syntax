module PostgresqlSyntax.Ast.MathOp where

import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
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
      [ ArrowLeftArrowRightMathOp <$ Parser.string' "<>",
        GreaterEqualsMathOp <$ Parser.string' ">=",
        ExclamationEqualsMathOp <$ Parser.string' "!=",
        LessEqualsMathOp <$ Parser.string' "<=",
        PlusMathOp <$ Parser.char '+',
        MinusMathOp <$ Parser.char '-',
        AsteriskMathOp <$ Parser.char '*',
        SlashMathOp <$ Parser.char '/',
        PercentMathOp <$ Parser.char '%',
        ArrowUpMathOp <$ Parser.char '^',
        ArrowLeftMathOp <$ Parser.char '<',
        ArrowRightMathOp <$ Parser.char '>',
        EqualsMathOp <$ Parser.char '='
      ]

instance Qc.Arbitrary MathOp where
  arbitrary = Qc.elements [minBound .. maxBound]
