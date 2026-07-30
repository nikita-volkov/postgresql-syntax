module PostgresqlSyntax.Ast.SimpleTypename where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Bit
import PostgresqlSyntax.Ast.Character
import PostgresqlSyntax.Ast.ConstDatetime
import PostgresqlSyntax.Ast.GenericType
import PostgresqlSyntax.Ast.Iconst
import PostgresqlSyntax.Ast.Interval
import PostgresqlSyntax.Ast.Numeric
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
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
  toTextBuilder settings = \case
    GenericTypeSimpleTypename a -> toTextBuilder settings a
    NumericSimpleTypename a -> toTextBuilder settings a
    BitSimpleTypename a -> toTextBuilder settings a
    CharacterSimpleTypename a -> toTextBuilder settings a
    ConstDatetimeSimpleTypename a -> toTextBuilder settings a
    ConstIntervalSimpleTypename a -> "INTERVAL" <> either (TextBuilders.suffixMaybe (toTextBuilder settings)) (mappend " " . TextBuilders.renderInParens . toTextBuilder settings) a
  parser settings =
    asum
      [ do
          Parsers.keyword "interval"
          Parser.endHead
          asum
            [ ConstIntervalSimpleTypename <$> Right <$> (Parsers.space *> Parsers.inParens (parser settings)),
              ConstIntervalSimpleTypename <$> Left <$> optional (Parsers.space *> parser settings)
            ],
        ConstDatetimeSimpleTypename <$> parser settings,
        NumericSimpleTypename <$> parser settings,
        BitSimpleTypename <$> parser settings,
        CharacterSimpleTypename <$> parser settings,
        GenericTypeSimpleTypename <$> parser settings
      ]

instance Qc.Arbitrary SimpleTypename where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ GenericTypeSimpleTypename <$> Qc.arbitrary,
        NumericSimpleTypename <$> Qc.arbitrary,
        BitSimpleTypename <$> Qc.arbitrary,
        CharacterSimpleTypename <$> Qc.arbitrary,
        ConstDatetimeSimpleTypename <$> Qc.arbitrary,
        ConstIntervalSimpleTypename <$> Qc.arbitrary
      ]
