-- |
-- Public surface of the @postgresql-syntax@ package.
--
-- Re-exports 'PostgresqlSyntax.IsAst', which provides the 'IsAst' class (the
-- per-type @parser@ \/ @toTextBuilder@ methods implemented in the
-- @PostgresqlSyntax.Ast.*@ node modules) together with the generic executors
-- 'parse', 'parseWithPosError' and 'toText'.
module PostgresqlSyntax
  ( toText,
    parse,
    parseWithPosError,

    -- * AST
    module PostgresqlSyntax.IsAst,
    module PostgresqlSyntax.Ast,
  )
where

import PostgresqlSyntax.Ast
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Extras
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified TextBuilder

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

-- |
-- Render a value to 'Text' via its 'toTextBuilder' method.
toText :: (IsAst a) => a -> Text
toText = TextBuilder.toText . toTextBuilder
