module PostgresqlSyntax.IsAst
  ( IsAst (..),
    toText,
    parse,
    parseWithPosError,
    parseWithSourcePosError,
  )
where

import qualified Data.Text as Text
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Extras
import PostgresqlSyntax.Prelude
import PostgresqlSyntax.Settings (Settings)
import qualified Text.Megaparsec as Megaparsec
import qualified TextBuilder

class IsAst a where
  toTextBuilder :: Settings -> a -> TextBuilder
  parser :: Settings -> Parser a

-- |
-- Render a value to 'Text' via its 'toTextBuilder' method.
toText :: (IsAst a) => Settings -> a -> Text
toText settings = TextBuilder.toText . toTextBuilder settings

-- |
-- Parse a 'Text' input with the type's 'parser', returning either a
-- pretty-printed error or the parsed value. The parser is chosen by the
-- caller's type inference (via the 'IsAst' constraint), so callers no longer
-- pass an explicit parser argument.
parse :: (IsAst a) => Settings -> Text -> Either Text a
parse settings = first Text.pack . Extras.run (Extras.totally (parser settings))

-- |
-- Like 'parse' but returns the structured error list (each error paired with
-- its byte offset) instead of a single pretty-printed message.
parseWithPosError :: (IsAst a) => Settings -> Text -> Either (NonEmpty (Int, Text)) a
parseWithPosError settings = first (fmap (second Text.pack)) . Extras.runParserWithErrorPos (Extras.totally (parser settings))

-- |
-- Like 'parseWithPosError' but pairs each error with its
-- 'Text.Megaparsec.SourcePos' instead of a raw byte offset.
parseWithSourcePosError :: (IsAst a) => Settings -> Text -> Either (NonEmpty (Megaparsec.SourcePos, Text)) a
parseWithSourcePosError settings = first (fmap (second Text.pack)) . Extras.runParserWithSourcePosError (Extras.totally (parser settings))
