-- |
-- Public surface of the @postgresql-syntax@ package.
--
-- Re-exports 'PostgresqlSyntax.IsAst', which provides the 'IsAst' class (the
-- per-type @parser@ \/ @toTextBuilder@ methods implemented in the
-- @PostgresqlSyntax.Ast.*@ node modules) together with the generic executors
-- 'run', 'runWithPosError', 'atEnd' and 'toText'.
module PostgresqlSyntax
  ( run,
    runWithPosError,
    atEnd,
    toText,

    -- * AST
    module PostgresqlSyntax.IsAst,
    module PostgresqlSyntax.Ast,
  )
where

import qualified HeadedMegaparsec
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
run :: (IsAst a) => Text -> Either String a
run = Extras.run parser

-- |
-- Like 'run' but returns the structured error list (each error paired with
-- its byte offset) instead of a single pretty-printed message.
runWithPosError :: (IsAst a) => Text -> Either (NonEmpty (Int, String)) a
runWithPosError = Extras.runParserWithErrorPos parser

-- |
-- Require the given parser to consume all remaining input (after optional
-- surrounding whitespace), asserting the parse reaches end-of-input.
atEnd :: Parser a -> Parser a
atEnd p = Extras.space *> p <* HeadedMegaparsec.endHead <* Extras.space <* Extras.eof

-- |
-- Render a value to 'Text' via its 'toTextBuilder' method.
toText :: (IsAst a) => a -> Text
toText = TextBuilder.toText . toTextBuilder
