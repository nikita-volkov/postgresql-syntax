# Upcoming

## Breaking

- Split the 5551-line `PostgresqlSyntax.Ast`/`.Parsing`/`.Rendering` into one
  module per AST node under `PostgresqlSyntax.Ast.*` (e.g.
  `PostgresqlSyntax.Ast.Ident`, `PostgresqlSyntax.Ast.AExpr`). `PostgresqlSyntax.Ast`
  remains a re-export root, so code importing only `PostgresqlSyntax.Ast` or
  `PostgresqlSyntax` and referring to types by name is unaffected; code that
  imported `PostgresqlSyntax.Parsing`/`.Rendering`/`.KeywordSet`/`.Validation`
  directly needs to migrate (see below).
- Removed `PostgresqlSyntax.Parsing` and `PostgresqlSyntax.Rendering`. Every
  type's parser and renderer are now the `parser`/`toTextBuilder` methods of
  its `IsAst` instance (exported from `PostgresqlSyntax`), and the top-level
  entry points moved to the `PostgresqlSyntax` module: `Parsing.run`/`.runWithPosError`
  became `PostgresqlSyntax.parse`/`.parseWithPosError`, generalized to work over
  any `IsAst` type rather than taking an explicit parser argument.
- `PostgresqlSyntax.KeywordSet` and `PostgresqlSyntax.Validation` are no longer
  part of the public API surface.
- Several former type aliases are now distinct ADTs/newtypes instead of bare
  `Either`/`Maybe`/primitive aliases: `SelectStmt`, `SelectClause`,
  `ExplicitRow`, and the primitive-wrapper newtypes `Sconst`, `Bconst`,
  `Xconst`, `Iconst`, `Fconst`, `Op`, `OptVarying`, `Timezone`,
  `IntervalSecond`, `OptOrdinality`. Code that pattern-matched these as
  `Either`/`Maybe`/`Text`/`Bool` directly needs to match on the new
  constructors instead.
- Removed the `SuffixQualOpAExpr` constructor of `AExpr`. It modelled the
  postfix operator production (`a_expr qual_Op`), which Postgres removed in
  version 14 — `x OPERATOR(pg_catalog.+#)` is a syntax error in every
  supported server version, so the parser no longer accepts it and the
  renderer can no longer emit it. Code pattern-matching or constructing
  `SuffixQualOpAExpr` needs to drop those cases. This also removes a family
  of round-trip failures: the rendering `<operand> <operator>` left the
  operator without a right-hand side, so reparsing swallowed whatever
  keyword followed (`PRECEDING`, `FOLLOWING`, `ROWS`, an implicit column
  alias, or the `?` of the `Typename` nullability extension) as its operand.

## Fixes

- Fix the `AExpr`/`BExpr` renderers (and related expression renderers) being
  precedence-naive: operators were concatenated without parenthesization, so a
  non-canonical but valid AST (e.g. `(NOT x) - y`) could render to SQL that
  re-parses to a different tree (`NOT (x - y)`). Renderers now parenthesize
  based on operator precedence, making render → parse round-trip safe for the
  full value space of each type, not just parser-canonical trees.

# v0.4.5.0

## Non-breaking

- Derive `Data` for every AST type in `PostgresqlSyntax.Ast` (#6). Enables
  generic (SYB-style) traversals and transformations over the syntax tree,
  e.g. for expression normalization.

# v0.4.4.0

## Fixes

- Fix parsing time growing exponentially with the nesting depth of an expression (#8).
  Inputs as small as `((((((((((((a + b))))))))))))` previously did not finish parsing;
  they now parse in well under a millisecond. Three grammar alternatives each parsed
  the content of a parenthesised group before discovering they did not apply, tripling
  the work per level of nesting; they are now left-factored so each group is parsed once.
  Parsing time is linear in input size, with a quadratic term in nesting depth alone
  (1000 characters of pure nesting take about a second; realistic input is unaffected).

## Non-breaking

- Redundant parentheses around a sub-select now parse to a canonical shape.
  `((select 1))` produces `WithParensSelectWithParens`; the equivalent
  `NoParensSelectWithParens` of a `SelectNoParens` carrying nothing but that same
  parenthesised select is no longer produced. The two rendered identically, so this
  only affects which of two equivalent trees you get back. Trees that carry a
  set operation, sort clause, limit or locking clause around the parenthesised
  select are unaffected.

# v0.4.3.2

## Fixes

- Fix keyword error reporting under megaparsec >=9.8 (#20)
- Fix OFFSET/aExpr round-trip failure for `OPERATOR(...)` prefix (#11, #22)
- Fix hedgehog generator for `type_function_name` to use its own keyword set
