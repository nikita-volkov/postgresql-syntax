# Investigation: `?` nullability extension vs. real Postgres `?` operator

**Status:** unresolved, needs a design decision before any fix is attempted.
**Started:** 2026-07-29, during a `/grill-me` session on the deferred `Op "?"`
round-trip bug listed in `docs/superpowers` handoff notes from the
2026-07-29 grammar-sync session.

## The bug as originally scoped

`Op "?"` (a generic user-defined binary operator, valid per real Postgres's
`scan.l`/`gram.y`) fails to round-trip whenever it's rendered immediately
after a `Typename`, e.g. as the operator in a `TypecastAExpr` chain:

```
(DEFAULT) :: SMALLINT ? (DEFAULT, DEFAULT)
```

fails to reparse: the parser reads `SMALLINT ?` as a (currently inert)
nullable-type marker and strands the rest of the expression. Known-affected
properties at last count: `ForLockingItem`, `SelectClause`, and (per wider
sampling with unseeded runs) anything that transitively contains an `AExpr`
adjacent to a `Typename` — `CallStmt`, `FuncArgExpr`, `FuncExpr`,
`SimpleSelect`, `UpdateStmt`, `WhenClause`, `WithClause`, etc. This is one of
the three known pre-existing bugs (`Op "?"`, `SubqueryAExpr` non-canonical
parens, `AllIndirectionEl`) that make the round-trip property suite flaky
under unpinned seeds — see the 2026-07-29 grammar-sync handoff and its
successor debugging session in this conversation's history.

## Code involved

- `library-internal/PostgresqlSyntax/Ast/Typename.hs` — the `Typename` AST
  node and its `parser`/`toTextBuilder`. Its Haddock documents the `?`
  suffix as **"Typename definition extended with custom question-marks for
  nullability specification"** — a non-standard, library-invented feature,
  not present in real Postgres.
- `library-internal/PostgresqlSyntax/Ast/Op.hs`, `Predicate.hs` (`opChar`),
  `CharSet.hs` (`op = "+-*/<>=~!@#%^&|`?"`), `Validation.hs` (`nonOp`) — `?`
  is a first-class, unexcluded operator character.
- `library-internal/PostgresqlSyntax/Ast/AExpr.hs:106,129` and
  `Helpers/Parsers.hs:186-190` (`symbolicBinOpExpr`) — where the binary
  operator is rendered/parsed, always padded (`" ? "`, mandatory space via
  `space *> op <* space`).
- `references/scan.l:364-366` — confirms real Postgres's own lexer treats
  `?` as an ordinary `op_chars` member, tokenized purely by character class,
  **with no whitespace requirement** (same as `+`, `-`, etc.).

## Mechanism of the existing bug (confirmed, not yet fixed)

`Typename.hs:41-52`:

```haskell
c <- Parsers.trueIfPresent (Parsers.space *> Parsers.char '?')
```

`Parsers.space` is the zero-or-more-whitespace variant (there's a separate
`space1` elsewhere for "at least one" — see `Helpers/Parsers.hs:153` for
contrast). So this line greedily eats `int?` **and** `int ?` as the
nullability marker, which collides with the binary operator's mandatory
`space *> op <* space`. Also independently broken: `toTextBuilder`
(`Typename.hs:39-40`) never emits the nullability flags at all, and
`Arbitrary` (`Typename.hs:56`) hard-codes them to `False` — so today the
nullability extension is entirely inert (can't be produced by the
generator, and if constructed by hand, doesn't round-trip through its own
renderer). This means the currently-observed round-trip failures are caused
by the *parser* swallowing a real `Op "?"`, not by any generated nullable
`Typename` actually exercising the feature.

## Why the obvious fix doesn't work

The first candidate fix considered: make the nullability marker require
**zero space** (bind tightly: `Typename` immediately followed by `?` with no
separator allowed, e.g. `int?`), while the binary operator continues to
require padding (`" ? "`) as it already does. The idea was to make the two
lexically disjoint — no space before `?` means nullability, a space means
operator.

**This does not work**, because real Postgres does not require whitespace
around operators at all. `?` tokenizes on character-class boundaries alone,
just like `+` or `-`. The user-supplied counterexample:

```sql
SELECT '{"a": 1, "b": 2}'::jsonb? 'b'
SELECT '{"a": 1, "b": 2}'::jsonb ? 'b'
```

Both of these are valid Postgres today (jsonb `?` "does this top-level key
exist" operator), and **both mean the same thing**: a cast to `jsonb`
followed by the binary `?` operator applied to `'b'`. There is no reading
in real Postgres where `jsonb?` (no space) means "nullable jsonb" — that
reading doesn't exist in the standard grammar at all; it's purely this
library's invention. Since real Postgres assigns `Typename` immediately
followed by `?` (space or no space) the single, unambiguous meaning "start
of a binary operator", **any** library syntax that also wants to claim `?`
directly after a `Typename` is squatting on syntax real Postgres already
uses for something else. No amount of whitespace-based disambiguation can
fix this while remaining compatible with real Postgres's own operator
syntax — the collision is with the *reference grammar itself*, not just
with an accident of this library's parser implementation.

## Open questions for a future session

1. **Is the nullability extension worth keeping at all**, given it
   permanently squats on syntax real Postgres has already assigned a
   different meaning to? It's currently 100% inert (see "Mechanism" above)
   — no test exercises it, and it doesn't even round-trip on its own. It may
   have shipped broken from the start and never been noticed.
2. If kept, **what syntax would not collide** with any valid real-Postgres
   continuation after a `Typename`? This needs a character/keyword that
   Postgres's own grammar never allows immediately after a typename in any
   context this library supports (not just typecasts — anywhere `Typename`
   is parsed). Candidates to evaluate: a reserved-word suffix (e.g. some
   `NULL`/`NOT NULL`-shaped keyword combination, though that risks its own
   collisions with column/domain constraint syntax elsewhere), or dropping
   the feature in favor of something that isn't positionally adjacent to
   `Typename` at all (e.g. a wrapper type/constructor instead of inline
   postfix syntax).
3. If the extension is dropped or redesigned, **is this a breaking change**
   requiring a `CHANGELOG.md` `## Breaking` entry and a major version bump?
   (Per `update-changelog` conventions used elsewhere in this repo.)
4. Separately from the syntax question: the dead `toTextBuilder`/`Arbitrary`
   no-op (flags never rendered, never generated) needs its own fix or
   removal regardless of which direction is chosen for the syntax itself.
5. Whatever direction is chosen, this is the same broad class of problem as
   the two other already-parked bugs (`SubqueryAExpr` canonical-parens,
   `AllIndirectionEl` in table-ref position) — all three cause the
   round-trip property suite to be flaky under unpinned QuickCheck seeds.
   Consider whether the suite should pin its seed in CI regardless of when/
   whether these get fixed, so failure counts stay reproducible.

## Not done in this session

No code was changed. This document exists so the collision, the ruled-out
fix, and the real reason it doesn't work are captured before the next
session re-derives them from scratch.

## Resolution (2026-07-29, same-day follow-up)

The open questions above are answered in
`docs/2026-07-29-nullability-settings-plan.md`, which was implemented in the
same session. Summary:

1. **Yes**, the nullability extension is worth keeping — it is used by the
   downstream `hasql-th` package's `text?[]?` spelling.
2. The collision is **contained, not resolved**: standard mode (`mempty`) is
   faithful Postgres where `?` is never a marker; extended mode
   (`nullabilityMarkers True`) claims only the **unspaced** `?` immediately
   after a `Typename`. This gives up exactly one real-Postgres spelling:
   `x::jsonb? 'b'`. The spaced form `x::jsonb ? 'b'` works in both modes.
3. Breaking: `parse x` → `parse mempty x`, version bumped to 0.5.0.0.
4. Renders emit `?` for both flags when markers are enabled.
5. `Arbitrary` keeps hard-coding both flags to `False` (valid under `mempty`).
