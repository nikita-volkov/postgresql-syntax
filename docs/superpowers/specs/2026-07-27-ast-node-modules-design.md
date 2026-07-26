# Design: One module per AST node cluster, via the Aggregator Namespace pattern

## Problem

`PostgresqlSyntax.Ast` (2340 lines, ~112 types), `PostgresqlSyntax.Parsing` (2290
lines), and `PostgresqlSyntax.Rendering` (921 lines) each mirror the same
112-node Postgres grammar as one flat module. Consequences:

- Hard to navigate: finding "the code for `SelectStmt`" means searching three
  giant files.
- Cannot compile in parallel: each of the three modules is a single
  compilation unit GHC must build serially, and `Parsing`/`Rendering` both
  depend on all of `Ast`.
- The public API exposes internal machinery (`KeywordSet`, `Validation`,
  `CharSet`, `Predicate`) that nothing outside the library's own parser/renderer
  actually needs from the outside.

## Constraint: mutual recursion

The 112 types are heavily mutually recursive (statements reference
expressions, expressions reference select-statements, select-statements
reference with-clauses that reference statements again, ...). Haskell modules
cannot have ordinary circular imports, so a literal one-module-per-type split
is not always possible without `.hs-boot` files per cycle edge.

**Decision:** cluster mutually-recursive types into one module per strongly
connected component (SCC) of the type-reference graph. Types with no
recursive dependency on their neighbors get their own module. This trades
some granularity for zero `.hs-boot` boilerplate. Computing the actual SCCs
(and therefore the exact module boundaries) is implementation work for the
plan phase, not part of this design — the strategy is what's fixed here, not
the resulting file list.

## Pattern: Aggregator Namespace, Variant 2 (Full Re-Export Root)

Per the project's Haskell standards
(`patterns/aggregator-namespace.md`): a set of uniform class implementors,
each in its own hidden module, re-exported through one root.

```
PostgresqlSyntax/Ast.hs                -- root: re-exports every cluster submodule
PostgresqlSyntax/Ast/<PrimaryType>.hs  -- one data/newtype (or one SCC cluster)
                                        -- + instance Syntactic <PrimaryType>
```

- The primary type of a cluster module is named after the module (existing
  naming convention).
- Cluster submodules are `other-modules` (hidden) — callers only ever import
  the `PostgresqlSyntax.Ast` root, per Variant 2.
- `PostgresqlSyntax.Parsing` and `PostgresqlSyntax.Rendering` cease to exist as
  standalone modules. Each node's parser function (from `Parsing.hs`) and
  render function (from `Rendering.hs`) move into that node's module as the
  two methods of one class instance.

## The class

Modeled on `SyntacticClass.Core`'s `Syntactic` class, adapted to this
package's own parser type (`HeadedParsec`, not Attoparsec — required for
left-recursion handling):

```haskell
-- PostgresqlSyntax.Syntactic
class Syntactic a where
  toTextBuilder :: a -> TextBuilder
  parser :: Parser a  -- Parser = HeadedParsec Void Text
```

Free functions currently loose in `Rendering.hs`/`Parsing.hs` (`toText`,
`run`, `runWithPosError`) move here, generalized over `Syntactic a =>` instead
of being hand-written per type.

## Public API surface

- **Exposed:** `PostgresqlSyntax.Ast` (root), `PostgresqlSyntax.Syntactic`.
- **Hidden** (moved to `other-modules`): `KeywordSet`, `Validation`,
  `CharSet`, `Predicate`, `Extras.*`, `Prelude`, and every `Ast.<Node>` cluster
  submodule. None of these have callers outside the library's own
  parsing/rendering code today.
- **Deleted:** `Parsing.hs` and `Rendering.hs` as top-level modules — their
  content is redistributed into the per-cluster modules.

## Breaking change

This is a breaking change to a published Hackage package (currently
0.4.4.0). Anything depending directly on `PostgresqlSyntax.Parsing`,
`.Rendering`, `.KeywordSet`, or `.Validation` breaks. Requires a major version
bump and a `CHANGELOG.md` entry under `Breaking`.

## Out of scope for this design

- The exact SCC partition / final list of cluster modules (computed during
  implementation).
- Whether `hedgehog-test/Main/Gen.hs` and `bench/*` need restructuring beyond
  updating imports to match the new module layout.
