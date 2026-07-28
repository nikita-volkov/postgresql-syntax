module PostgresqlSyntax.Ast.SimpleTypename where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Bit
import PostgresqlSyntax.Ast.Character
import PostgresqlSyntax.Ast.ConstDatetime
import PostgresqlSyntax.Ast.GenericType
import PostgresqlSyntax.Ast.Iconst
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.Interval
import PostgresqlSyntax.Ast.Numeric
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
          Parser.endHead
          asum
            [ ConstIntervalSimpleTypename <$> Right <$> (Parser.space *> inParens parser),
              ConstIntervalSimpleTypename <$> Left <$> optional (Parser.space *> parser)
            ],
        ConstDatetimeSimpleTypename <$> parser,
        NumericSimpleTypename <$> parser,
        BitSimpleTypename <$> parser,
        CharacterSimpleTypename <$> parser,
        GenericTypeSimpleTypename <$> parser
      ]

instance Qc.Arbitrary SimpleTypename where
  arbitrary =
    Qc.oneof
      [ GenericTypeSimpleTypename <$> Qc.arbitrary,
        NumericSimpleTypename <$> Qc.arbitrary,
        BitSimpleTypename <$> Qc.arbitrary,
        CharacterSimpleTypename <$> Qc.arbitrary,
        ConstDatetimeSimpleTypename <$> Qc.arbitrary,
        ConstIntervalSimpleTypename <$> Qc.arbitrary
      ]
