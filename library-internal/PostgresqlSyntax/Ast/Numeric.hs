module PostgresqlSyntax.Ast.Numeric where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.ExprList
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- Numeric:
--   | INT_P
--   | INTEGER
--   | SMALLINT
--   | BIGINT
--   | REAL
--   | FLOAT_P opt_float
--   | DOUBLE_P PRECISION
--   | DECIMAL_P opt_type_modifiers
--   | DEC opt_type_modifiers
--   | NUMERIC opt_type_modifiers
--   | BOOLEAN_P
-- opt_float:
--   | '(' Iconst ')'
--   | EMPTY
-- opt_type_modifiers:
--   | '(' expr_list ')'
--   | EMPTY
-- @
data Numeric
  = IntNumeric
  | IntegerNumeric
  | SmallintNumeric
  | BigintNumeric
  | RealNumeric
  | FloatNumeric (Maybe Int64)
  | DoublePrecisionNumeric
  | DecimalNumeric (Maybe ExprList)
  | DecNumeric (Maybe ExprList)
  | NumericNumeric (Maybe ExprList)
  | BooleanNumeric
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Numeric where
  toTextBuilder settings = \case
    IntNumeric -> "INT"
    IntegerNumeric -> "INTEGER"
    SmallintNumeric -> "SMALLINT"
    BigintNumeric -> "BIGINT"
    RealNumeric -> "REAL"
    FloatNumeric a -> "FLOAT" <> TextBuilders.suffixMaybe (TextBuilders.renderInParens . TextBuilder.int64Dec) a
    DoublePrecisionNumeric -> "DOUBLE PRECISION"
    DecimalNumeric a -> "DECIMAL" <> TextBuilders.suffixMaybe (TextBuilders.renderInParens . toTextBuilder settings) a
    DecNumeric a -> "DEC" <> TextBuilders.suffixMaybe (TextBuilders.renderInParens . toTextBuilder settings) a
    NumericNumeric a -> "NUMERIC" <> TextBuilders.suffixMaybe (TextBuilders.renderInParens . toTextBuilder settings) a
    BooleanNumeric -> "BOOLEAN"
  parser settings =
    asum
      [ IntegerNumeric <$ Parsers.keyword "integer",
        IntNumeric <$ Parsers.keyword "int",
        SmallintNumeric <$ Parsers.keyword "smallint",
        BigintNumeric <$ Parsers.keyword "bigint",
        RealNumeric <$ Parsers.keyword "real",
        FloatNumeric <$> (Parsers.keyword "float" *> Parser.endHead *> optional (Parsers.space *> Parsers.inParens Parsers.decimal)),
        DoublePrecisionNumeric <$ Parsers.keyphrase "double precision",
        DecimalNumeric <$> (Parsers.keyword "decimal" *> Parser.endHead *> optional (Parsers.space *> Parsers.inParens (parser settings))),
        DecNumeric <$> (Parsers.keyword "dec" *> Parser.endHead *> optional (Parsers.space *> Parsers.inParens (parser settings))),
        NumericNumeric <$> (Parsers.keyword "numeric" *> Parser.endHead *> optional (Parsers.space *> Parsers.inParens (parser settings))),
        BooleanNumeric <$ Parsers.keyword "boolean"
      ]

instance Qc.Arbitrary Numeric where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ pure IntNumeric,
        pure IntegerNumeric,
        pure SmallintNumeric,
        pure BigintNumeric,
        pure RealNumeric,
        -- The @Iconst@ here is parsed via 'Parsers.decimal' (unsigned), so,
        -- unlike a plain 'Int64', it must never be negative — mirroring
        -- 'PostgresqlSyntax.Ast.IntervalSecond'\'s own @nonNegative@.
        FloatNumeric <$> Qc.oneof [pure Nothing, Just <$> nonNegativeInt64],
        pure DoublePrecisionNumeric,
        DecimalNumeric <$> Qc.arbitrary,
        DecNumeric <$> Qc.arbitrary,
        NumericNumeric <$> Qc.arbitrary,
        pure BooleanNumeric
      ]
    where
      nonNegativeInt64 = Qc.sized (\n -> Qc.choose (0, cap n))
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n
