module PostgresqlSyntax.Ast.ConstCharacter where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Character
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.Extras.TextBuilder (int64Dec)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

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
  toTextBuilder (ConstCharacter a b) = toTextBuilder a <> suffixMaybe (renderInParens . int64Dec) b
  parser = ConstCharacter <$> (parser <* endHead) <*> optional (space *> inParens decimal)

instance Arbitrary ConstCharacter where
  arbitrary = ConstCharacter <$> arbitrary <*> arbitrary
