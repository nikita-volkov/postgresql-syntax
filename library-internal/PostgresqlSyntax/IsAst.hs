module PostgresqlSyntax.IsAst
  ( IsAst (..),
    toText,
    parse,
    parseWithPosError,
  )
where

import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Extras
import PostgresqlSyntax.Prelude
import qualified TextBuilder

class IsAst a where
  toTextBuilder :: a -> TextBuilder
  parser :: Parser a

-- |
-- Render a value to 'Text' via its 'toTextBuilder' method.
toText :: (IsAst a) => a -> Text
toText = TextBuilder.toText . toTextBuilder

-- |
-- Parse a 'Text' input with the type's 'parser', returning either a
-- pretty-printed error or the parsed value. The parser is chosen by the
-- caller's type inference (via the 'IsAst' constraint), so callers no longer
-- pass an explicit parser argument.
parse :: (IsAst a) => Text -> Either String a
parse = Extras.run (Extras.totally parser)

-- |
-- Like 'parse' but returns the structured error list (each error paired with
-- its byte offset) instead of a single pretty-printed message.
parseWithPosError :: (IsAst a) => Text -> Either (NonEmpty (Int, String)) a
parseWithPosError = Extras.runParserWithErrorPos (Extras.totally parser)
