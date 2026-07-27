-- |
-- Thin compatibility surface kept for external consumers (the test suites
-- and benchmarks in this package) that still refer to specific parsers by
-- name. All per-type parsing logic now lives colocated with its type under
-- "PostgresqlSyntax.Ast" (see the @PostgresqlSyntax.Ast.<Type>@ modules'
-- @instance IsAst@ blocks) — each binding here is a one-line delegation to
-- the generic 'PostgresqlSyntax.IsAst.parser'.
module PostgresqlSyntax.Parsing
  ( run,
    runWithPosError,
    aExpr,
    preparableStmt,
    typename,
    sconst,
    tableRef,
    selectNoParens,
    selectWithParens,
  )
where

import PostgresqlSyntax.Ast
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Extras
import PostgresqlSyntax.IsAst (Parser, parser)
import PostgresqlSyntax.Prelude

run :: Parser a -> Text -> Either String a
run = Extras.run

runWithPosError :: Parser a -> Text -> Either (NonEmpty (Int, String)) a
runWithPosError = Extras.runParserWithErrorPos

aExpr :: Parser AExpr
aExpr = parser

preparableStmt :: Parser PreparableStmt
preparableStmt = parser

typename :: Parser Typename
typename = parser

sconst :: Parser Sconst
sconst = parser

tableRef :: Parser TableRef
tableRef = parser

selectNoParens :: Parser SelectNoParens
selectNoParens = parser

selectWithParens :: Parser SelectWithParens
selectWithParens = parser
