# Upcoming

## Breaking

- Fixed the parsed tree shape of `UNION`/`INTERSECT`/`EXCEPT` chains to match Postgres's actual associativity and precedence (`gram.y`'s `%left UNION EXCEPT` / `%left INTERSECT`, INTERSECT binding tighter): `a EXCEPT b EXCEPT c` now nests left instead of right, and `a INTERSECT b UNION c` now roots at `UNION` instead of `INTERSECT`. Rendered text is unaffected; only `SimpleSelect`'s parsed/canonical tree shape for such chains changes (#30).
- Fixed a further associativity bug in the same fold: a run of two or more consecutive `INTERSECT`s (e.g. `a INTERSECT b INTERSECT c`) nested right instead of left, contradicting `gram.y`'s `%left INTERSECT`. Parsed/canonical tree shape changes for such chains; rendered text is unaffected (#34).

## Non-breaking

- `PostgresqlSyntax.Algebra`'s `LeftRecursion base ext item` class (internal-library-only, not part of the public `PostgresqlSyntax` API) is now a single-method `ExtendedBy base ext` — drops the unread `item` type parameter: `extension`/`applyExtension`/`foldExtensions` are gone in favor of one `parseExtensions` method; `nonRecursiveParser` → `parseBase`; `parseLeftRecursive` → `parseMaybeExtended`; `leftRecursionProperties` → `extendedByProperties`. `JoinedTableExtension` is deleted (#33).
- `TableRef.hs` and `FuncApplicationParams.hs` now have explicit export lists, exporting only their types and instances (#30).

# v0.5.0.1

## Fixes

- Added doc-files to the .cabal-file. README.md, LICENSE and CHANGELOG.md.

# v0.5.0.0

## Breaking

- `IsAst` class methods `parser`/`toTextBuilder` now take a `Settings` parameter
  to control parse/render options (e.g. nullability `?` markers). Migration:
  `parse x` -> `parse mempty x`, `toText x` -> `toText mempty x`,
  `parseWithPosError x` -> `parseWithPosError mempty x`. New public module
  `PostgresqlSyntax.Settings` provides the `Settings` type and the
  `nullabilityMarkers` constructor.
- Nullability `?` markers in `Typename` are now opt-in via
  `nullabilityMarkers True`. In standard mode (`mempty`) they are not
  recognized - both flags parse as `False` and a literal `?` in their
  position is a parse error. Markers no longer accept a preceding space:
  `int ?` -> `int?`. Extended mode gives up one spelling of real Postgres:
  `x::jsonb? 'b'` (unspaced jsonb key-existence operator); use
  `x::jsonb ? 'b'` or `jsonb_exists(x, 'b')`.
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
  version 14 - `x OPERATOR(pg_catalog.+#)` is a syntax error in every
  supported server version, so the parser no longer accepts it and the
  renderer can no longer emit it. Code pattern-matching or constructing
  `SuffixQualOpAExpr` needs to drop those cases. This also removes a family
  of round-trip failures: the rendering `<operand> <operator>` left the
  operator without a right-hand side, so reparsing swallowed whatever
  keyword followed (`PRECEDING`, `FOLLOWING`, `ROWS`, an implicit column
  alias, or - in the postfix case specifically - the `?` of the `Typename`
  nullability extension) as its operand. Only the postfix-triggered `?`
  swallow is fixed by this; `Op "?"` colliding with the nullability
  extension as a *binary* operator is a separate, still-open issue.

## Fixes

- Fix the `AExpr`/`BExpr` renderers (and related expression renderers) being
  precedence-naive: operators were concatenated without parenthesization, so a
  non-canonical but valid AST (e.g. `(NOT x) - y`) could render to SQL that
  re-parses to a different tree (`NOT (x - y)`). Renderers now parenthesize
  based on operator precedence, making render -> parse round-trip safe for the
  full value space of each type, not just parser-canonical trees.

# v0.4.5.0

## Non-breaking

- Derive `Data` for every AST type in `PostgresqlSyntax.Ast` (#6). Enables
  generic (SYB-style) traversals and transformations over the syntax tree,
  e.g. for expression normalization.
- Add `parseWithSourcePosError`, like `parseWithPosError` but pairing each
  error with a `Text.Megaparsec.SourcePos` instead of a raw `Int` byte
  offset, so callers (e.g. `hasql-th`, see
  [nikita-volkov/hasql-th#35](https://github.com/nikita-volkov/hasql-th/issues/35))
  can report line/column positions without recomputing them from the input.

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
