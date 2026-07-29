-- |
-- Rendering helpers shared by 2+ AST node modules.
module PostgresqlSyntax.Helpers.TextBuilders where

import qualified Data.Text.Encoding as Text
import qualified PostgresqlSyntax.Extras.NonEmpty as NonEmpty
import PostgresqlSyntax.Prelude
import qualified TextBuilder

commaNonEmpty :: (a -> TextBuilder) -> NonEmpty a -> TextBuilder
commaNonEmpty = NonEmpty.intersperseFoldMap ", "

spaceNonEmpty :: (a -> TextBuilder) -> NonEmpty a -> TextBuilder
spaceNonEmpty = NonEmpty.intersperseFoldMap " "

lexemes :: [TextBuilder] -> TextBuilder
lexemes = mconcat . intersperse " "

optLexemes :: [Maybe TextBuilder] -> TextBuilder
optLexemes = lexemes . catMaybes

renderInParens :: TextBuilder -> TextBuilder
renderInParens a = "(" <> a <> ")"

renderInBrackets :: TextBuilder -> TextBuilder
renderInBrackets a = "[" <> a <> "]"

prefixMaybe :: (a -> TextBuilder) -> Maybe a -> TextBuilder
prefixMaybe a = foldMap (flip mappend " " . a)

suffixMaybe :: (a -> TextBuilder) -> Maybe a -> TextBuilder
suffixMaybe a = foldMap (mappend " " . a)

renderAllOrDistinct :: Bool -> TextBuilder
renderAllOrDistinct = \case
  False -> "ALL"
  True -> "DISTINCT"
