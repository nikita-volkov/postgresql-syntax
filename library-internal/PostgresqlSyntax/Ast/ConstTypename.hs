module PostgresqlSyntax.Ast.ConstTypename where

import PostgresqlSyntax.Ast.Bit (Bit)
import PostgresqlSyntax.Ast.ConstCharacter
import PostgresqlSyntax.Ast.ConstDatetime
import PostgresqlSyntax.Ast.Numeric
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- ConstTypename:
--   | Numeric
--   | ConstBit
--   | ConstCharacter
--   | ConstDatetime
-- @
data ConstTypename
  = NumericConstTypename Numeric
  | ConstBitConstTypename Bit
  | ConstCharacterConstTypename ConstCharacter
  | ConstDatetimeConstTypename ConstDatetime
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ConstTypename where
  toTextBuilder settings = \case
    NumericConstTypename a -> toTextBuilder settings a
    ConstBitConstTypename a -> toTextBuilder settings a
    ConstCharacterConstTypename a -> toTextBuilder settings a
    ConstDatetimeConstTypename a -> toTextBuilder settings a
  parser settings =
    asum
      [ NumericConstTypename <$> parser settings,
        ConstBitConstTypename <$> parser settings,
        ConstCharacterConstTypename <$> parser settings,
        ConstDatetimeConstTypename <$> parser settings
      ]

instance Qc.Arbitrary ConstTypename where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ NumericConstTypename <$> Qc.arbitrary,
        ConstBitConstTypename <$> Qc.arbitrary,
        ConstCharacterConstTypename <$> Qc.arbitrary,
        ConstDatetimeConstTypename <$> Qc.arbitrary
      ]
