# Design: One module per AST node, via the Aggregator Namespace pattern

## Problem

`PostgresqlSyntax.Ast` (2340 lines, 112 primary types + 61 type aliases),
`PostgresqlSyntax.Parsing` (2290 lines), and `PostgresqlSyntax.Rendering` (921
lines) each mirror the same Postgres grammar as one flat module. Consequences:

- Hard to navigate: finding "the code for `SelectStmt`" means searching three
  giant files.
- Cannot compile in parallel: each of the three modules is a single
  compilation unit GHC must build serially, and `Parsing`/`Rendering` both
  depend on all of `Ast`.
- The public API exposes internal machinery (`KeywordSet`, `Validation`,
  `CharSet`, `Predicate`) that nothing outside the library's own parser/renderer
  actually needs from the outside.

## Constraint: mutual recursion, quantified

The type-reference graph of `Ast.hs` was computed directly (Tarjan SCC over
112 primary types + 61 type aliases, 173 declarations total, 233 edges within
the largest component):

- One dominant SCC of 123 declarations (85 of the 112 primary types + 38 type
  aliases) — essentially all of statements, select, expressions, joins,
  windows, and typenames.
- 50 completely independent singletons (mostly enums and simple wrappers:
  `Ident`, `Iconst`, `ArrayBounds`, ...).
- Nothing in between — no medium-sized clusters.

**Decision:** split down to one module per individual type, everywhere,
including inside the giant SCC — using `.hs-boot` files to break the cycles
that a naive one-module-per-type split can't resolve on its own. A DFS-order
heuristic shows this is tractable, not runaway: breaking the giant SCC only
requires boot files at the true recursion hubs of the grammar (expressions
embedding subqueries, statements embedding `WITH` clauses that embed
statements again, table refs embedding joins that embed select clauses).
Concretely, 8 types need a companion `.hs-boot`:

```
AExpr, ArrayExpr, BExpr, FuncExprCommonSubexpr,
SelectNoParens, SelectWithParens, TableRef, WithClause
```

These boot files are **abstract only** — bare `data X`, plus the `parser` /
`toTextBuilder` signatures. Verified against the current `Rendering.hs` /
`Parsing.hs`: every constructor-level pattern match on these 8 types' shapes
happens exclusively inside that type's own render/parse function; every other
module only ever calls the generic `parser`/`toTextBuilder` on them or holds
them as an opaque field. So no constructor list needs duplicating in the boot
file, which keeps the ongoing hand-sync cost low.

The exact module boundaries for the remaining ~100+ non-hub types are
implementation work for the plan phase, not part of this design.

## Pattern: Aggregator Namespace, Variant 2 (Full Re-Export Root)

Per the project's Haskell standards
(`patterns/aggregator-namespace.md`): a set of uniform class implementors,
each in its own hidden module, re-exported through one root.

```
PostgresqlSyntax/Ast.hs                -- root: re-exports every cluster submodule
PostgresqlSyntax/Ast/<PrimaryType>.hs  -- one data/newtype/ADT
                                        -- + instance IsAst <PrimaryType>
                                        -- + instance Arbitrary <PrimaryType>
PostgresqlSyntax/Ast/<PrimaryType>.hs-boot  -- only for the 8 recursion hubs listed above
```

- The primary type of a cluster module is named after the module (existing
  naming convention).
- Cluster submodules are hidden — callers only ever import the
  `PostgresqlSyntax.Ast` root, per Variant 2.
- `PostgresqlSyntax.Parsing` and `PostgresqlSyntax.Rendering` cease to exist as
  standalone modules. Each node's parser function (from `Parsing.hs`) and
  render function (from `Rendering.hs`) move into that node's module as the
  two methods of one class instance.

## Handling of the 61 type aliases

The original design didn't address bare `type` aliases at all. Resolved
per-alias by checking against `references/gram.y` and the current
`Parsing.hs`/`Rendering.hs` implementation, rather than by blanket rule:

- **Aliasing another already-AST-modeled type** (e.g. `ColId = Ident`,
  `Name = ColId`, `TypeFunctionName = Ident`, `Collate = AnyName`) — stays a
  bare `type` alias, re-exported from `Ast` for documentation value, no
  module or instance of its own. No inlining/collapsing — even pure renames
  are kept distinct (a strict "collapse to the root type" reading would lose
  grammar-position documentation, e.g. `CursorName` → `Name` → `ColId` →
  `Ident` collapsing away entirely).
- **Aliasing `NonEmpty X`** (e.g. `ExprList`, `TargetList`, `WhenClauseList`)
  — becomes a `newtype` with its own `IsAst` instance. Verified there's no
  shortcut generic `instance IsAst (NonEmpty a)`: separators aren't uniform
  (most lists are comma-separated via `commaNonEmpty`/`sep1 commaSeparator`,
  but `WhenClauseList` inside `CASE` is space-separated) — so each list type
  really does need its own instance.
- **Aliasing `Either A B`** — becomes a proper ADT with two named
  constructors following the existing suffix convention, not a wrapped
  `Either`. E.g. `SelectStmt = Either SelectNoParens SelectWithParens`
  becomes `data SelectStmt = NoParensSelectStmt SelectNoParens |
  WithParensSelectStmt SelectWithParens`. (A bare `Either`/`NonEmpty` alias
  also can't carry a targeted instance without an orphan/overlap problem, so
  this is required, not just stylistic.)
- **Aliasing `Maybe X`** — becomes a proper ADT with present/absent
  constructors, matching the grammar's two productions. E.g.
  `ExplicitRow = Maybe ExprList` (`explicit_row: ROW '(' expr_list ')' | ROW
  '(' ')'`) becomes `data ExplicitRow = EmptyExplicitRow | ExprListExplicitRow
  ExprList`.
- **Aliasing a raw/foreign primitive** (`Text`, `Int64`, `Double`, `Bool`,
  or `Maybe` of one of these) — becomes a `newtype`, required rather than
  stylistic: `Sconst`, `Bconst`, and `Xconst` are all `= Text` but parse
  differently (string-constant escaping vs. bit-string vs. hex-string), so
  they cannot all instance bare `Text`. Same conflict confirmed for `Bool`
  (`OptVarying` vs. `Timezone`, unrelated grammar productions per `gram.y`).
  Affected: `Sconst`, `Bconst`, `Xconst`, `Iconst`, `Fconst`, `Op`,
  `OptVarying`, `Timezone`, `IntervalSecond`.

## The class: `IsAst`

Modeled on `SyntacticClass.Core`'s pattern, adapted to this package's own
parser type (`HeadedParsec`, not Attoparsec — required for left-recursion
handling), and named to match the project's own `Is*` aggregator-namespace
convention (`IsStatement`, `IsScalar`, `IsCodec`):

```haskell
-- PostgresqlSyntax.IsAst
class IsAst a where
  toTextBuilder :: a -> TextBuilder
  parser :: Parser a  -- Parser = HeadedParsec Void Text
```

Lives in its own module, re-exported from the `PostgresqlSyntax` root
alongside `PostgresqlSyntax.Ast`.

Free functions currently loose in `Rendering.hs`/`Parsing.hs` (`toText`,
`run`, `runWithPosError`, `atEnd`) move into the top-level `PostgresqlSyntax`
surface module, generalized over `IsAst a =>` instead of being hand-written
per type. Confirmed these four are the only genuinely generic,
non-per-node functions in the current `Parsing.hs`/`Rendering.hs` — everything
else (e.g. `preparableStmt`) is a per-node function that becomes an `IsAst`
instance method.

## Public API surface

Split into two library components (Cabal internal library +
`reexported-modules`, not just a documentation convention — enforced by Cabal
itself):

```
library postgresql-syntax-internal
  -- everything: Ast root + all cluster submodules, KeywordSet, Validation,
  -- CharSet, Predicate, Extras.*, Prelude, PostgresqlSyntax, PostgresqlSyntax.IsAst
  -- Only visible to sibling components within this same package.

library
  -- the public one, published to Hackage under the package name
  reexported-modules:
    PostgresqlSyntax.Ast as PostgresqlSyntax.Ast,
    PostgresqlSyntax as PostgresqlSyntax
  build-depends: postgresql-syntax-internal
```

- **Exposed (public):** `PostgresqlSyntax` (surface: `IsAst`, `run`,
  `runWithPosError`, `atEnd`, `toText`), `PostgresqlSyntax.Ast` (root).
- **Hidden** (internal-library only): `KeywordSet`, `Validation`, `CharSet`,
  `Predicate`, `Extras.*`, `Prelude`, `PostgresqlSyntax.IsAst`'s
  implementation module, and every `Ast.<Node>` cluster submodule.
  Confirmed via `hasql-th` (the package's stated primary consumer) that it
  only imports `PostgresqlSyntax.Ast`/`.Parsing`/`.Rendering` — never
  `KeywordSet`/`Validation` — so hiding them breaks no external caller.
  Note: this package's *own* `bench/Main.hs` and (pre-migration)
  `hedgehog-test/Main/Gen.hs` do import `KeywordSet`/`Validation` directly —
  those in-tree components must switch their `build-depends` to
  `postgresql-syntax-internal` rather than the public library.
- **Deleted:** `Parsing.hs` and `Rendering.hs` as top-level modules — their
  content is redistributed into the per-node modules.

## Testing

- `Arbitrary` instances (QuickCheck) are colocated in each `Ast.<Node>`
  module, next to the data declaration and the `IsAst` instance — replacing
  the current hand-written, flat `hedgehog-test/Main/Gen.hs` (971 lines).
  Accepted consequence: `QuickCheck` becomes a transitive build dependency of
  the *published* `postgresql-syntax` package (not just of test components),
  since the internal library's modules (which the public library re-exports)
  reference it directly.
- `tasty-test` and `hedgehog-test` are both retired in favor of a single
  `hspec` + `hspec-discover` test-suite. Round-trip parse/render properties
  (currently in `hedgehog-test/Main.hs`) move to `Test.Hspec.QuickCheck`
  props against the colocated `Arbitrary` instances; the example-based cases
  (currently in `tasty-test/Main.hs`, hand-picked SQL snippets via
  `Parsing.run`) port directly to hspec's `describe`/`it`.
- `bench/keyword-bench` and `bench/nesting-bench` are unaffected in framework
  (still `criterion`), but `keyword-bench` currently shares
  `hedgehog-test/Main/Gen.hs` for input generation and imports `KeywordSet`
  directly — both need updating to the new per-node `Arbitrary` instances and
  the `postgresql-syntax-internal` dependency.

## Breaking change

This is a breaking change to a published Hackage package (currently
0.4.5.0). Anything depending directly on `PostgresqlSyntax.Parsing`,
`.Rendering`, `.KeywordSet`, or `.Validation` breaks. Requires a major version
bump and a `CHANGELOG.md` entry under `Breaking`.

## Out of scope for this design

- The exact module list for the ~100+ non-hub types (computed during
  implementation).
- Exact `.hs-boot` file contents beyond "abstract, no constructors" (drafted
  during implementation).
- Whether `nesting-bench` needs restructuring beyond updating imports and
  dependencies to match the new module layout and internal library.
