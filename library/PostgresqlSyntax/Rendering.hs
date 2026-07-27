-- |
-- Thin compatibility surface kept for external consumers (the test suites
-- and benchmarks in this package) that still refer to specific renderers by
-- name. All per-type rendering logic now lives colocated with its type
-- under "PostgresqlSyntax.Ast" (see the @PostgresqlSyntax.Ast.<Type>@
-- modules' @instance IsAst@ blocks) — each binding here is a one-line
-- delegation to the generic 'PostgresqlSyntax.IsAst.toTextBuilder'.
module PostgresqlSyntax.Rendering
  ( toText,
    aExpr,
    typename,
    tableRef,
    preparableStmt,
  )
where

import PostgresqlSyntax.Ast
import PostgresqlSyntax.IsAst (toTextBuilder)
import TextBuilder (TextBuilder, toText)

aExpr :: AExpr -> TextBuilder
aExpr = toTextBuilder

typename :: Typename -> TextBuilder
typename = toTextBuilder

tableRef :: TableRef -> TextBuilder
tableRef = toTextBuilder

preparableStmt :: PreparableStmt -> TextBuilder
preparableStmt = toTextBuilder
