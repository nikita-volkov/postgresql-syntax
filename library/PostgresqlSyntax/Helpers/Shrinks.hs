-- |
-- Shrink helpers shared by 2+ AST node modules.
module PostgresqlSyntax.Helpers.Shrinks where

import qualified Data.List as List
import qualified Data.Text as Text
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- Shrinks a 'Text' value by shrinking it as a 'String' (dropping\/simplifying
-- characters).
text :: Text -> [Text]
text = fmap Text.pack . Qc.shrink . Text.unpack

-- |
-- Shrinks a 'Text' value, excluding empty results.
nonEmptyText :: Text -> [Text]
nonEmptyText = List.filter (not . Text.null) . text

-- |
-- Shrinks the tail of a 'Text' value, keeping its first character fixed.
-- Useful for text with constraints on the first character (e.g. identifiers),
-- where shrinking it away could produce an invalid result.
textTail :: Text -> [Text]
textTail a =
  case Text.uncons a of
    Nothing -> []
    Just (head, tail) ->
      Text.cons head <$> nonEmptyText tail
