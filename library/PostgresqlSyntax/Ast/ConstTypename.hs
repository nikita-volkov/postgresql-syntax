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
  toTextBuilder = \case
    NumericConstTypename a -> toTextBuilder a
    ConstBitConstTypename a -> toTextBuilder a
    ConstCharacterConstTypename a -> toTextBuilder a
    ConstDatetimeConstTypename a -> toTextBuilder a
  parser =
    asum
      [ NumericConstTypename <$> parser,
        ConstBitConstTypename <$> parser,
        ConstCharacterConstTypename <$> parser,
        ConstDatetimeConstTypename <$> parser
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
