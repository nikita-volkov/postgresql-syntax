# Plan: settings-parameterised parsing/rendering, tight `?` nullability markers

**Status:** agreed, ready to implement.
**Supersedes the open questions in:** `docs/2026-07-29-nullability-question-mark-collision.md`
(that document stays as the investigation record; it gets a resolution section
at the end of this work).

## Decision

**Contain the collision, don't resolve it.**

- Standard mode (the default) is faithful Postgres: `?` is only ever an operator
  character, and a nullability marker is a parse error.
- Extended mode claims the *unspaced* `?` immediately after a `Typename` for
  nullability. It gives up exactly one spelling of real Postgres: `x::jsonb? 'b'`
  (unspaced jsonb key-existence operator). `x::jsonb ? 'b'` keeps working in both
  modes. Documented workaround for the lost spelling: `jsonb_exists(x, 'b')` —
  the same answer PgJDBC gives for its identical `?` collision.

Rationale for tight binding: `symbolicBinOpExpr` parses operators as
`space *> op <* space` with `space` being *zero*-or-more, and renders them
always padded (`AExpr.hs:128`). So a generated `Op "?"` always round-trips in
both modes, and the marker wins only in the one spelling nobody writes by hand.

## API changes (breaking — 0.4.5.0 → 0.5.0.0)

### New public module `PostgresqlSyntax.Settings`

```haskell
-- abstract: constructor and field selector NOT exported
data Settings = Settings {optNullabilityMarkers :: Maybe Bool}
  deriving (Show, Eq)

instance Semigroup Settings where   -- per field, right operand wins
  a <> b = Settings {optNullabilityMarkers = optNullabilityMarkers b <|> optNullabilityMarkers a}

instance Monoid Settings where
  mempty = Settings Nothing         -- == standard Postgres

-- the only exported constructor
nullabilityMarkers :: Bool -> Settings

-- internal, not re-exported from the facade
resolveNullabilityMarkers :: Settings -> Bool
resolveNullabilityMarkers = fromMaybe False . optNullabilityMarkers
```

One type only — no separate resolved `Config`. Defaults are applied at the use
site. Since `Settings` is abstract, a `Config` indirection can be introduced
later without a public break.

### `IsAst`

```haskell
class IsAst a where
  toTextBuilder :: Settings -> a -> TextBuilder
  parser        :: Settings -> Parser a

toText            :: (IsAst a) => Settings -> a -> Text
parse             :: (IsAst a) => Settings -> Text -> Either String a
parseWithPosError :: (IsAst a) => Settings -> Text -> Either (NonEmpty (Int, String)) a
```

No compatibility aliases. Migration is `parse x` → `parse mempty x`.

### Behaviour matrix

| input | `mempty` | `nullabilityMarkers True` |
| --- | --- | --- |
| `$1 :: text?` | parse error (operator with no rhs) | nullable `text` |
| `$1 :: text[]?` | parse error | `text` array, nullable array |
| `$1 :: text?[]?` | parse error | nullable `text`, nullable array |
| `x::jsonb ? 'b'` | jsonb operator | jsonb operator |
| `x::jsonb? 'b'` | jsonb operator | nullable `jsonb`, then stranded `'b'` → error |

Rendering under `mempty` **silently drops** a `True` flag. This preserves the
invariant that standard mode never emits non-Postgres SQL, and keeps the
`Settings` parameter of `toTextBuilder` load-bearing.

## Work items

1. **`PostgresqlSyntax/Settings.hs`** (internal lib) — the type above. Export
   from the `PostgresqlSyntax` facade.
2. **`PostgresqlSyntax/IsAst.hs`** — add the `Settings` parameter to both class
   methods and to `toText`/`parse`/`parseWithPosError`.
3. **Thread the parameter through all 143 `IsAst` instances** (~969
   `parser`/`toTextBuilder` occurrences). Purely mechanical: each instance binds
   the settings and passes it down. Helpers in `Helpers/Parsers.hs` that take a
   `Parser` or have an `IsAst` constraint (`typecastExpr`, `qualOpExpr`,
   `symbolicBinOpExpr`, `iconstOrFconst`, …) gain the same parameter.
4. **`Ast/Typename.hs`** — the real change:
   - marker parser becomes `Parsers.trueIfPresent (Parsers.char '?')` (drop the
     leading `Parsers.space`), guarded on `resolveNullabilityMarkers`; when the
     setting is off, the marker is not attempted at all and both flags parse as
     `False`.
   - `toTextBuilder` starts emitting `?` for both flags when the setting is on
     (today it never emits them at all — half the reason the feature is inert).
   - correct the Haddock: state that the markers are opt-in via `Settings`, that
     they bind tightly, and name the `jsonb ?` trade-off.
   - `Arbitrary` keeps hard-coding both flags to `False` — `Arbitrary` means
     "valid under `mempty`".
5. **Tests**
   - `Helpers/Specs.hs`: settings-aware variants of `itParses`/`itRejects`
     (and `itSatisfiesIsAst` passes `mempty`).
   - `hspec-test/Ast/TypenameSpec.hs`: a `describe "extended"` block covering
     the full matrix above — all four flag combinations parse, round-trip, and
     are rejected under `mempty`; plus the four `jsonb ?` / `jsonb?` cross-mode
     operator cases.
   - `hedgehog-test/Main.hs`: pass `mempty` at the two call sites. Generators
     unchanged.
6. **`CHANGELOG.md`** — `## Breaking` under a new `0.5.0.0`:
   - `parse`, `parseWithPosError`, `toText` take `Settings`; migration
     `parse x` → `parse mempty x`.
   - nullability `?` markers are opt-in via `nullabilityMarkers True`.
   - markers no longer accept a preceding space: `int ?` → `int?`.
7. **`postgresql-syntax.cabal`** — version `0.5.0.0`, expose
   `PostgresqlSyntax.Settings` from the internal lib.
8. **`docs/2026-07-29-nullability-question-mark-collision.md`** — append a
   resolution section pointing here; the open questions are now answered.

## Out of scope

- **hasql-th.** Updated separately once 0.5.0.0 is on Hackage: switch to
  `parse (nullabilityMarkers True)`, regenerate the `InputTypeList` doctests
  (their `text?[]?` spelling is already tight-compatible), bump the bound.
- **The other two parked round-trip bugs** — `SubqueryAExpr` non-canonical
  parens and `AllIndirectionEl` in table-ref position. The suite stays
  seed-sensitive because of them; this change removes only the `Op "?"` source
  of flakiness (in both modes).

## Verified before writing this plan

- `ArrayBounds.toTextBuilder` = `intersperseFoldMap " "`, no leading space, and
  `TypenameArrayDimensions`' bounds parser is `Parsers.space *> parser` with
  optional space — so `text?` `<>` `[]` renders and reparses as `text?[]`.
- The `ARRAY` form renders as `" ARRAY"` (leading space) and parses after
  `space1`, so no abutment with a tight `?`.
- All binary-operator renderings pad; prefix operators only occur at an
  operand's start. No renderer emits a `Typename` directly abutting a
  `?`-initial token.
