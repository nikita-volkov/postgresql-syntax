# References

- Grammar file (primary source of info): https://github.com/postgres/postgres/blob/master/src/backend/parser/gram.y
- Keywords file: https://github.com/postgres/postgres/blob/master/src/include/parser/kwlist.h
- Scanner: https://github.com/postgres/postgres/blob/master/src/backend/parser/scan.l
- Other files (for extra info): https://github.com/postgres/postgres/tree/master/src/backend/parser

# `Arbitrary` instances for `Ast` types

Each `PostgresqlSyntax.Ast.*` module's `instance Qc.Arbitrary <Type>` must terminate at size 0 and stay proportionally bounded as size grows. Don't reach for blanket `Qc.scale`/`Qc.resize` guesswork on a whole generator or `oneof` block — apply `Qc.downscale` (from `PostgresqlSyntax.Extras.QuickCheck`) individually, per field, using this rule:

- An `arbitrary` call gets wrapped in `Qc.downscale` if and only if the value it produces is:
  1. the type being defined itself (direct self-recursion) — including nested inside a container like `Maybe X`, `[X]`, `NonEmpty X`, or `Either X Y`, in which case the whole container-producing call gets one `Qc.downscale`, not each element, OR
  2. a type imported into that module via `import {-# SOURCE #-} ...`.
- Every other field (plain, non-recursive, non-`SOURCE` types) stays plain `Qc.arbitrary`/`arbitrary` — no downscaling.
- This holds even for fields whose type is only *mutually* recursive with the type being defined through an ordinary (non-`SOURCE`) import: that recursion is already terminated on the other side of the cycle, at whichever module has the `SOURCE` import breaking it. Downscaling both sides would over-shrink.
- Every `Arbitrary` instance must terminate at size 0 — i.e. escape every recursive strongly-connected component and yield a small/leaf value, not an unbounded random walk. Use the `Qc.terminatingMaybe` helper (also in `PostgresqlSyntax.Extras.QuickCheck`) for `Maybe X` fields where `X` is self-recursive or `SOURCE`-imported — it forces `Nothing` at small sizes instead of relying on the default `Maybe` instance's non-size-aware `frequency`.
- Reference implementations: `PostgresqlSyntax.Ast.AExpr`, `PostgresqlSyntax.Ast.AnyOperator`, `PostgresqlSyntax.Ast.ExprList`, `PostgresqlSyntax.Ast.CaseExpr`.
