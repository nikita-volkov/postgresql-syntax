module PostgresqlSyntax.Ast.ConstCharacter where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.Character
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- ConstCharacter:
--   | CharacterWithLength
--   | CharacterWithoutLength
-- CharacterWithLength:
--   | character '(' Iconst ')'
-- CharacterWithoutLength:
--   | character
-- @
data ConstCharacter = ConstCharacter Character (Maybe Int64)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ConstCharacter where
  toTextBuilder settings (ConstCharacter a b) = toTextBuilder settings a <> TextBuilders.suffixMaybe (TextBuilders.renderInParens . TextBuilder.int64Dec) b
  parser settings = ConstCharacter <$> (parser settings <* Parser.endHead) <*> optional (Parsers.space *> Parsers.inParens Parsers.decimal)

instance Qc.Arbitrary ConstCharacter where
  shrink = Qc.genericShrink

  -- The length here is parsed via 'Parsers.decimal' (unsigned), so it must
  -- never be negative — mirroring 'PostgresqlSyntax.Ast.IntervalSecond'\'s
  -- own @nonNegative@.
  arbitrary = ConstCharacter <$> arbitrary <*> Qc.oneof [pure Nothing, Just <$> nonNegativeInt64]
    where
      nonNegativeInt64 = Qc.sized (\n -> Qc.choose (0, cap n))
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n
