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
  arbitrary = ConstCharacter <$> arbitrary <*> arbitrary
