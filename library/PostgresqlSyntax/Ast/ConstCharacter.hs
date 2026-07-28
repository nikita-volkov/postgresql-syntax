module PostgresqlSyntax.Ast.ConstCharacter where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Character
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder (ConstCharacter a b) = toTextBuilder a <> suffixMaybe (renderInParens . TextBuilder.int64Dec) b
  parser = ConstCharacter <$> (parser <* Parser.endHead) <*> optional (Parser.space *> inParens Parser.decimal)

instance Qc.Arbitrary ConstCharacter where
  shrink = Qc.genericShrink

  -- \| The length here is parsed via 'Parser.decimal' (unsigned), so it must
  -- never be negative — mirroring 'PostgresqlSyntax.Ast.IntervalSecond'\'s
  -- own @nonNegative@.
  arbitrary = ConstCharacter <$> arbitrary <*> Qc.oneof [pure Nothing, Just <$> nonNegativeInt64]
    where
      nonNegativeInt64 = Qc.sized (\n -> Qc.choose (0, cap n))
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n
