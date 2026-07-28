module PostgresqlSyntax.Ast.Numeric where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder = \case
    IntNumeric -> "INT"
    IntegerNumeric -> "INTEGER"
    SmallintNumeric -> "SMALLINT"
    BigintNumeric -> "BIGINT"
    RealNumeric -> "REAL"
    FloatNumeric a -> "FLOAT" <> suffixMaybe (renderInParens . TextBuilder.int64Dec) a
    DoublePrecisionNumeric -> "DOUBLE PRECISION"
    DecimalNumeric a -> "DECIMAL" <> suffixMaybe (renderInParens . toTextBuilder) a
    DecNumeric a -> "DEC" <> suffixMaybe (renderInParens . toTextBuilder) a
    NumericNumeric a -> "NUMERIC" <> suffixMaybe (renderInParens . toTextBuilder) a
    BooleanNumeric -> "BOOLEAN"
  parser =
    asum
      [ IntegerNumeric <$ keyword "integer",
        IntNumeric <$ keyword "int",
        SmallintNumeric <$ keyword "smallint",
        BigintNumeric <$ keyword "bigint",
        RealNumeric <$ keyword "real",
        FloatNumeric <$> (keyword "float" *> Parser.endHead *> optional (Parser.space *> inParens Parser.decimal)),
        DoublePrecisionNumeric <$ keyphrase "double precision",
        DecimalNumeric <$> (keyword "decimal" *> Parser.endHead *> optional (Parser.space *> inParens parser)),
        DecNumeric <$> (keyword "dec" *> Parser.endHead *> optional (Parser.space *> inParens parser)),
        NumericNumeric <$> (keyword "numeric" *> Parser.endHead *> optional (Parser.space *> inParens parser)),
        BooleanNumeric <$ keyword "boolean"
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
        -- The @Iconst@ here is parsed via 'Parser.decimal' (unsigned), so,
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
