module PostgresqlSyntax.Ast.SimpleTypename where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Bit
import PostgresqlSyntax.Ast.Character
import PostgresqlSyntax.Ast.ConstDatetime
import PostgresqlSyntax.Ast.GenericType
import PostgresqlSyntax.Ast.Iconst
import PostgresqlSyntax.Ast.Interval
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.Numeric
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

-- |
-- ==== References
-- @
-- SimpleTypename:
--   | GenericType
--   | Numeric
--   | Bit
--   | Character
--   | ConstDatetime
--   | ConstInterval opt_interval
--   | ConstInterval '(' Iconst ')'
-- ConstInterval:
--   | INTERVAL
-- @
data SimpleTypename
  = GenericTypeSimpleTypename GenericType
  | NumericSimpleTypename Numeric
  | BitSimpleTypename Bit
  | CharacterSimpleTypename Character
  | ConstDatetimeSimpleTypename ConstDatetime
  | ConstIntervalSimpleTypename (Either (Maybe Interval) Iconst)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SimpleTypename where
  toTextBuilder = \case
    GenericTypeSimpleTypename a -> toTextBuilder a
    NumericSimpleTypename a -> toTextBuilder a
    BitSimpleTypename a -> toTextBuilder a
    CharacterSimpleTypename a -> toTextBuilder a
    ConstDatetimeSimpleTypename a -> toTextBuilder a
    ConstIntervalSimpleTypename a -> "INTERVAL" <> either (suffixMaybe toTextBuilder) (mappend " " . renderInParens . toTextBuilder) a
  parser =
    asum
      [ do
          keyword "interval"
          endHead
          asum
            [ ConstIntervalSimpleTypename <$> Right <$> (space *> inParens parser),
              ConstIntervalSimpleTypename <$> Left <$> optional (space *> parser)
            ],
        ConstDatetimeSimpleTypename <$> parser,
        NumericSimpleTypename <$> parser,
        BitSimpleTypename <$> parser,
        CharacterSimpleTypename <$> parser,
        GenericTypeSimpleTypename <$> parser
      ]

instance Arbitrary SimpleTypename where
  arbitrary =
    oneof
      [ GenericTypeSimpleTypename <$> arbitrary,
        NumericSimpleTypename <$> arbitrary,
        BitSimpleTypename <$> arbitrary,
        CharacterSimpleTypename <$> arbitrary,
        ConstDatetimeSimpleTypename <$> arbitrary,
        ConstIntervalSimpleTypename <$> arbitrary
      ]
