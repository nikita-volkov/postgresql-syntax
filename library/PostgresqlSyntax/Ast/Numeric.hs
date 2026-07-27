module PostgresqlSyntax.Ast.Numeric where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.Extras.TextBuilder (int64Dec)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

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
    FloatNumeric a -> "FLOAT" <> suffixMaybe (renderInParens . int64Dec) a
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
        FloatNumeric <$> (keyword "float" *> endHead *> optional (space *> inParens decimal)),
        DoublePrecisionNumeric <$ keyphrase "double precision",
        DecimalNumeric <$> (keyword "decimal" *> endHead *> optional (space *> inParens parser)),
        DecNumeric <$> (keyword "dec" *> endHead *> optional (space *> inParens parser)),
        NumericNumeric <$> (keyword "numeric" *> endHead *> optional (space *> inParens parser)),
        BooleanNumeric <$ keyword "boolean"
      ]

instance Arbitrary Numeric where
  arbitrary =
    oneof
      [ pure IntNumeric,
        pure IntegerNumeric,
        pure SmallintNumeric,
        pure BigintNumeric,
        pure RealNumeric,
        FloatNumeric <$> arbitrary,
        pure DoublePrecisionNumeric,
        DecimalNumeric <$> arbitrary,
        DecNumeric <$> arbitrary,
        NumericNumeric <$> arbitrary,
        pure BooleanNumeric
      ]
