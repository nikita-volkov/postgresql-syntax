# AST Node Modules Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split `PostgresqlSyntax.Ast`/`.Parsing`/`.Rendering` (5551 lines, 3 flat modules) into one module per AST node (Aggregator Namespace, Variant 2), per `docs/superpowers/specs/2026-07-27-ast-node-modules-design.md`.

**Architecture:** Every `data`/`newtype` AST type (and every alias that needs its own instance) moves into `PostgresqlSyntax/Ast/<Type>.hs`, carrying its `IsAst` instance (parser + renderer, unified) and its `Arbitrary` instance. `PostgresqlSyntax.Ast` becomes a re-export root plus the ~27 bare `type` aliases that don't need modules. `Parsing.hs`, `Rendering.hs`, and `hedgehog-test/Main/Gen.hs` are deleted; their content is redistributed. The package splits into an internal library (everything) and a public library (`PostgresqlSyntax.Ast` + `PostgresqlSyntax` re-exported only).

**Tech Stack:** GHC/Cabal, `headed-megaparsec`, `text-builder`, QuickCheck (new dependency), `hspec`/`hspec-discover` (replaces `tasty`+`hedgehog`).

## Global Constraints

- Primary type of each node module is named after the module (`PostgresqlSyntax.Ast.Ident` exports `Ident`), per `patterns/aggregator-namespace.md` Variant 2.
- Every node module: `data`/`newtype` decl + `instance IsAst <Type>` (`toTextBuilder`, `parser`) + `instance Arbitrary <Type>` (QuickCheck) — all colocated, replacing the current three-way split across `Ast.hs`/`Parsing.hs`/`Rendering.hs`/`hedgehog-test/Main/Gen.hs`.
- Function/generator names are unchanged across the move — `Parsing.hs`'s `ident`, `Rendering.hs`'s `ident`, and `hedgehog-test/Main/Gen.hs`'s `ident` all become the `parser`/`toTextBuilder`/`arbitrary` methods of `instance IsAst Ident`/`instance Arbitrary Ident` in `Ast/Ident.hs`. Confirmed 1:1 name correspondence across all three files for every type (spot-checked `ident`, `sconst`, `exprList`, `selectStmt`, `explicitRow`).
- This is a pure move: no behavior changes. Verification per task is "package still builds and the full existing test suite (`tasty-test` + `hedgehog-test`, until Task 17 retires them) still passes" — not new failing tests, since no new behavior is introduced.
- Every task must leave `git status` clean of a broken build: run `cabal build` (and, until they're retired, `cabal test`) before moving to the next task.
- Breaking change to a published Hackage package (0.4.5.0 → next major). CHANGELOG entry under `Breaking` required (Task 20).

## Correction to the design doc (verified by computation, not part of the original spec)

The design doc names 8 recursion hubs needing `.hs-boot` files: `AExpr, ArrayExpr, BExpr, FuncExprCommonSubexpr, SelectNoParens, SelectWithParens, TableRef, WithClause`. I reconstructed the full 173-node type-dependency graph from the current `Ast.hs` and ran Tarjan's SCC algorithm on it (script kept at `/private/tmp/claude-501/.../scratchpad/scc.py` for this session, not part of the repo). It reproduces the design doc's own numbers exactly (one 123-member SCC, 50 singletons, nothing in between), which gives confidence the edge list is right. But cutting only those 8 hubs' outgoing edges does **not** fully break the SCC into a DAG: `SimpleSelect` and `SelectClause` form a residual 2-cycle (`SimpleSelect`'s `BinSimpleSelect` constructor embeds two `SelectClause` fields; `SelectClause = Either SimpleSelect SelectWithParens`) that doesn't route through any of the 8 listed hubs. Verified directly against `library/PostgresqlSyntax/Ast.hs:287-292`:

```haskell
data SimpleSelect
  = NormalSimpleSelect (Maybe Targeting) (Maybe IntoClause) (Maybe FromClause) (Maybe WhereClause) (Maybe GroupClause) (Maybe HavingClause) (Maybe WindowClause)
  | ValuesSimpleSelect ValuesClause
  | TableSimpleSelect RelationExpr
  | BinSimpleSelect SelectBinOp SelectClause (Maybe Bool) SelectClause
```

**This plan adds `SimpleSelect` as a 9th boot hub**, consistent with the existing convention that a hub is the "meaty" record type, not the thin `Either`/`Maybe` wrapper sitting next to it (`SelectNoParens`/`SelectWithParens` are hubs, `SelectStmt` — their `Either` wrapper — is not; by the same logic `SimpleSelect` is the hub, `SelectClause` stays a plain, non-booted `Either`-style ADT). With this correction, cutting the 9 hubs' outgoing edges leaves the remaining 114 SCC members as a genuine DAG (verified: 0 residual cross-module cycles; the only leftover cycles are `GroupByItem` and `JoinedTable` self-loops, which are single-file recursion and never a problem).

## Target module inventory

173 top-level declarations in `Ast.hs` today: 118 `data`/`newtype`, 55 `type` aliases. Per the design doc's per-alias rules:

- **27 bare aliases** — stay as plain `type` lines directly in `PostgresqlSyntax/Ast.hs` (the root), unchanged in text, no module of their own. They resolve automatically once the type they name is modularized, because the root module imports every node module unqualified.
- **146 module-producing declarations** get `PostgresqlSyntax/Ast/<Type>.hs`: 118 data/newtype + 16 `NonEmpty`-alias newtypes + 2 `Either`-alias ADTs (`SelectStmt`, `SelectClause`) + 1 `Maybe`-alias ADT (`ExplicitRow`) + 10 primitive-alias newtypes (`Sconst`, `Bconst`, `Xconst`, `Iconst`, `Fconst`, `Op`, `OptVarying`, `Timezone`, `IntervalSecond`, and `OptOrdinality` — the last one aliases `Bool` like `OptVarying`/`Timezone` do, and the design doc's "Affected" list missed it; applying the doc's own general rule ("aliasing a raw/foreign primitive... becomes a newtype, required rather than stylistic") consistently includes it).
- Of the 146, **9 are the recursion hubs** (`AExpr, ArrayExpr, BExpr, FuncExprCommonSubexpr, SelectNoParens, SelectWithParens, SimpleSelect, TableRef, WithClause`), each shipping a companion `.hs-boot`.
- The remaining 137 module-producing declarations split into **40 singletons** (zero dependency on the giant SCC — computed and confirmed: no singleton depends on any SCC member) and **97 non-hub SCC members**.

Bare aliases (leave in place in `Ast.hs`, add an import for their target once it's modularized — no task references these individually beyond that):

```
ReturningClause, UsingClause, IntoClause, HavingClause, ExistingWindowName, PartitionClause,
RepeatableClause, ColDefList, CollateClause, WhereClause, WithinGroupClause, FilterClause,
OverlayPlacing, SubstrFrom, SubstrFor, CaseArg, CaseDefault, ConstBit, ColId, ColLabel, Name,
CursorName, AttrName, TypeModifiers, Collate, Class, TypeFunctionName
```

## The mechanical transformation pattern

Every batch task in this plan applies one of six transformations. Each is fully worked below with real, current source (verified against the repo at the start of this plan — line numbers will drift as earlier batches edit the files, so locate by **function/type name**, not line number, when executing later tasks).

### Pattern A — plain `data` type (majority case)

Source (`Ast.hs:1830-1831`, `Parsing.hs:1991`/`1982`, `Rendering.hs:826-829`, `hedgehog-test/Main/Gen.hs:901`):

```haskell
-- Ast.hs
data Ident = QuotedIdent Text | UnquotedIdent Text
  deriving (Show, Generic, Eq, Ord, Data)

-- Parsing.hs
quotedName = filter (const "Empty name") (not . Text.null) (quotedString '"') & fmap QuotedIdent
ident = quotedName <|> keywordNameByPredicate (not . Predicate.keyword)

-- Rendering.hs
ident = \case
  QuotedIdent a -> char7 '"' <> text (Text.replace "\"" "\"\"" a) <> char7 '"'
  UnquotedIdent a -> text a

-- hedgehog-test/Main/Gen.hs
ident = identWithSet mempty   -- (helper stays local to Gen infra, see Task 15)
```

Target (`Ast/Ident.hs`):

```haskell
module PostgresqlSyntax.Ast.Ident where

import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (...)  -- copy the subset of hides Parsing.hs/Rendering.hs needed for this type's own code
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import qualified PostgresqlSyntax.Predicate as Predicate
import Test.QuickCheck (Arbitrary (..))
-- + whatever Gen.hs's `ident`/`identWithSet` needed

data Ident = QuotedIdent Text | UnquotedIdent Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Ident where
  toTextBuilder = \case
    QuotedIdent a -> char7 '"' <> text (Text.replace "\"" "\"\"" a) <> char7 '"'
    UnquotedIdent a -> text a
  parser = quotedName <|> keywordNameByPredicate (not . Predicate.keyword)
    where
      quotedName = filter (const "Empty name") (not . Text.null) (quotedString '"') & fmap QuotedIdent

instance Arbitrary Ident where
  arbitrary = ... -- ported from Gen.hs's `identWithSet mempty`, see Task 15 for the shared helper's new home
```

Any local helper used by only this one parser/renderer (e.g. `quotedName` above) moves with it, becoming a private `where`-bound or top-level (non-exported) function in the same module. Helpers shared by 2+ types (e.g. `keywordNameByPredicate`, `iconstOrFconst`) move to a shared internal module — see Task 3.

### Pattern B — primitive-alias newtype

Source (`Ast.hs:1591`, `Parsing.hs:1731`, `Rendering.hs:743`, `Gen.hs:841`):

```haskell
-- Ast.hs
type Sconst = Text
-- Parsing.hs
sconst = quotedString '\'' <|> dollarQuotedSconst
-- Rendering.hs
sconst a = "'" <> text (Text.replace "'" "''" a) <> "'"
-- Gen.hs
sconst = text (Range.exponential 0 1000) unicode
```

Target (`Ast/Sconst.hs`):

```haskell
module PostgresqlSyntax.Ast.Sconst where

newtype Sconst = Sconst Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Sconst where
  toTextBuilder (Sconst a) = "'" <> text (Text.replace "'" "''" a) <> "'"
  parser = Sconst <$> (quotedString '\'' <|> dollarQuotedSconst)

instance Arbitrary Sconst where
  arbitrary = Sconst <$> Test.QuickCheck.Gen... -- ported from Gen.hs's `text (Range.exponential 0 1000) unicode` (Hedgehog Range -> QuickCheck size-bounded generator)
```

Every call site elsewhere that pattern-matched a bare `Text` where `Sconst` used to be gets a `coerce`/newtype-unwrap added — audit at the call site during the batch that removes `Sconst`'s old `type` line (Task 9).

### Pattern C — `NonEmpty X` alias newtype

Source (`Ast.hs:997`, `Parsing.hs:1027`, `Rendering.hs:456`, `Gen.hs:434`):

```haskell
-- Ast.hs
type ExprList = NonEmpty AExpr
-- Parsing.hs
exprList = sep1 commaSeparator aExpr
-- Rendering.hs
exprList = commaNonEmpty aExpr
-- Gen.hs
exprList = nonEmpty (Range.exponential 1 7) aExpr
```

Target (`Ast/ExprList.hs`) — this one references `AExpr`, a boot hub, so it uses the `{-# SOURCE #-}` import:

```haskell
module PostgresqlSyntax.Ast.ExprList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.IsAst

newtype ExprList = ExprList (NonEmpty AExpr)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ExprList where
  toTextBuilder (ExprList a) = commaNonEmpty toTextBuilder a
  parser = ExprList <$> sep1 commaSeparator parser

instance Arbitrary ExprList where
  arbitrary = ExprList <$> ... -- ported from Gen.hs, 1-7 elements
```

Note `AExpr`'s own `.hs-boot` only declares `data AExpr` + `instance IsAst AExpr` signatures (Task 16) — `ExprList` never pattern-matches `AExpr`'s constructors, only calls `parser`/`toTextBuilder` on it (generic `commaNonEmpty`/`sep1` calls), so the boot's abstract signature is sufficient. This holds for all 9 hubs per the design doc's own verification.

### Pattern D — `Either A B` alias ADT

Source (`Ast.hs:231`, `Parsing.hs:330`, `Rendering.hs:144-146`):

```haskell
-- Ast.hs
type SelectStmt = Either SelectNoParens SelectWithParens
-- Parsing.hs
selectStmt = Left <$> selectNoParens <|> Right <$> selectWithParens
-- Rendering.hs
selectStmt = \case
  Left a -> selectNoParens a
  Right a -> selectWithParens a
```

Target (`Ast/SelectStmt.hs`), per the design doc's own example naming (`NoParens`/`WithParens` prefix + `SelectStmt` suffix):

```haskell
module PostgresqlSyntax.Ast.SelectStmt where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectNoParens (SelectNoParens)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import PostgresqlSyntax.IsAst

data SelectStmt
  = NoParensSelectStmt SelectNoParens
  | WithParensSelectStmt SelectWithParens
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SelectStmt where
  toTextBuilder = \case
    NoParensSelectStmt a -> toTextBuilder a
    WithParensSelectStmt a -> toTextBuilder a
  parser = NoParensSelectStmt <$> parser <|> WithParensSelectStmt <$> parser

instance Arbitrary SelectStmt where
  arbitrary = oneof [NoParensSelectStmt <$> arbitrary, WithParensSelectStmt <$> arbitrary]
```

`SelectClause` (Task 11) follows the identical pattern against `SimpleSelect`/`SelectWithParens`.

### Pattern E — `Maybe X` alias ADT

Source (`Ast.hs:1231`, `Parsing.hs:1322`, `Rendering.hs:601`):

```haskell
-- Ast.hs
type ExplicitRow = Maybe ExprList
-- Parsing.hs
explicitRow = keyword "row" *> space *> inParens (optional exprList)
-- Rendering.hs
explicitRow a = "ROW " <> inParens (foldMap exprList a)
```

Target (`Ast/ExplicitRow.hs`), per the design doc's own example naming:

```haskell
module PostgresqlSyntax.Ast.ExplicitRow where

data ExplicitRow
  = EmptyExplicitRow
  | ExprListExplicitRow ExprList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ExplicitRow where
  toTextBuilder a = "ROW " <> inParens (case a of EmptyExplicitRow -> mempty; ExprListExplicitRow a -> toTextBuilder a)
  parser = keyword "row" *> space *> inParens (maybe EmptyExplicitRow ExprListExplicitRow <$> optional parser)

instance Arbitrary ExplicitRow where
  arbitrary = oneof [pure EmptyExplicitRow, ExprListExplicitRow <$> arbitrary]
```

### Pattern F — recursion hub (with `.hs-boot`)

`.hs-boot` (`Ast/AExpr.hs-boot`, applies to all 9 hubs analogously):

```haskell
module PostgresqlSyntax.Ast.AExpr where

data AExpr

instance IsAst AExpr
```

`Ast/AExpr.hs` itself follows Pattern A (it's a plain `data` type) once every other module it needs has either a real `.hs` or, for the 8 *other* hubs it touches, their own `.hs-boot`. Created last (Task 16), after all 137 non-hub module-producing types already have real `.hs` files, so `AExpr.hs`'s own imports of e.g. `CExpr`, `Typename`, `Row` etc. are all real, ordinary imports — only imports of the other 8 hubs (where a genuine mutual cycle exists) use `{-# SOURCE #-}`.

## Task 1: `IsAst` class module

**Files:**
- Create: `library/PostgresqlSyntax/IsAst.hs`

**Interfaces:**
- Produces: `class IsAst a where { toTextBuilder :: a -> TextBuilder; parser :: Parser a }`, and re-exports `Parser` — every later task's `instance IsAst <Type>` block depends on this.

- [ ] **Step 1: Write the module**

```haskell
module PostgresqlSyntax.IsAst
  ( IsAst (..),
    Parser,
  )
where

import HeadedMegaparsec (HeadedParsec)
import PostgresqlSyntax.Prelude
import TextBuilder (TextBuilder)

type Parser = HeadedParsec Void Text

class IsAst a where
  toTextBuilder :: a -> TextBuilder
  parser :: Parser a
```

- [ ] **Step 2: Add to cabal `other-modules`** (it's not public yet — Task 18 splits the library and moves it to the internal component's exposed set)

Edit `postgresql-syntax.cabal`, add `PostgresqlSyntax.IsAst` to the `library` stanza's `other-modules`, alphabetically after `PostgresqlSyntax.Extras.TextBuilder`.

- [ ] **Step 3: Build**

Run: `cabal build postgresql-syntax`
Expected: succeeds (module is currently unused, so this only proves it parses/typechecks standalone).

- [ ] **Step 4: Commit**

```bash
git add library/PostgresqlSyntax/IsAst.hs postgresql-syntax.cabal
git commit -m "Add IsAst class module"
```

## Task 2: `Test.QuickCheck` dependency + `Prelude` re-export

**Files:**
- Modify: `postgresql-syntax.cabal` (add `QuickCheck` to `library`'s `build-depends`)
- Modify: `library/PostgresqlSyntax/Prelude.hs`

**Interfaces:**
- Consumes: none.
- Produces: `Test.QuickCheck.Arbitrary`/`arbitrary`/`oneof`/`elements`/etc. available unqualified from `PostgresqlSyntax.Prelude` — every later `instance Arbitrary <Type>` block depends on this being in scope without a per-module import line (matches this Prelude's existing style of re-exporting third-party APIs).

- [ ] **Step 1: Add the dependency**

In `postgresql-syntax.cabal`'s `library` stanza `build-depends`, insert alphabetically:

```
QuickCheck >=2.14 && <3,
```

- [ ] **Step 2: Re-export from Prelude**

In `library/PostgresqlSyntax/Prelude.hs`, add near the other `Data.*`/third-party `as Exports` imports:

```haskell
import Test.QuickCheck as Exports (Arbitrary (..), Gen, choose, elements, listOf, oneof, sized, vectorOf)
```

Adjust the explicit import list as later tasks discover more QuickCheck combinators are needed (e.g. `Range`-equivalents) — QuickCheck has no `Range` type, generators use `Gen`'s own `sized`/`choose`/`scale` instead of Hedgehog's `Range.exponential`; each batch task that ports a `Gen.hs` generator translates the Hedgehog combinator to the QuickCheck equivalent inline (e.g. `Range.exponential 1 7` on a list becomes `resize 7 (listOf1 ...)` or an explicit `choose (1,7) >>= \n -> vectorOf n ...`; there is no single mechanical substitution, translate the *distribution intent* — "1 to 7 elements, exponentially weighted toward the low end" — not the API surface).

- [ ] **Step 3: Build**

Run: `cabal build postgresql-syntax`
Expected: succeeds.

- [ ] **Step 4: Commit**

```bash
git add postgresql-syntax.cabal library/PostgresqlSyntax/Prelude.hs
git commit -m "Add QuickCheck dependency for colocated Arbitrary instances"
```

## Task 3: Shared parsing/rendering helper module

Several helpers in `Parsing.hs`/`Rendering.hs` are used by 2+ node types and don't belong to any single node module: the generic combinators (`inSpace`, `commaSeparator`, `dotSeparator`, `inBrackets`, `inBracketsCont`, `inParens`, `inParensCont`, `inParensWithLabel`, `inParensWithClause`, `trueIfPresent`, `quotedString`, `dollarQuotedSconst`, `commaNonEmpty`, `spaceNonEmpty`, `lexemes`, `optLexemes`, `prefixMaybe`, `suffixMaybe`), the keyword-matching infra (`keywordNameFromSet`, `keywordNameByPredicate`, `anyKeyword`, `keyword`, `keyphrase`), and the cross-type-family helpers (`typecastExpr`, `plusedExpr`, `minusedExpr`, `qualOpExpr`, `symbolicBinOpExpr`, `iconstOrFconst`, `toByteString`).

**Files:**
- Create: `library/PostgresqlSyntax/Ast/Internal.hs`

**Interfaces:**
- Consumes: `PostgresqlSyntax.IsAst.Parser`.
- Produces: the ~24 helper functions above, unqualified, importable by any node module. Every later batch task that needs one of these imports `PostgresqlSyntax.Ast.Internal`.

- [ ] **Step 1: Create the module and move the helpers**

Copy each of the listed functions verbatim from `Parsing.hs`/`Rendering.hs` into `library/PostgresqlSyntax/Ast/Internal.hs` under `module PostgresqlSyntax.Ast.Internal where`, with the same imports those functions currently need (`Control.Applicative.Combinators`, `HeadedMegaparsec`, `qualified PostgresqlSyntax.KeywordSet as KeywordSet`, `qualified PostgresqlSyntax.Predicate as Predicate`, `qualified Text.Megaparsec as Megaparsec`, `qualified TextBuilder`, `PostgresqlSyntax.IsAst`). **Do not delete them from `Parsing.hs`/`Rendering.hs` yet** — those two files keep working, unmodified in behavior, until Task 17 deletes them wholesale; this task only makes the helpers available in their new home for the nodes that need them starting with Task 5.

- [ ] **Step 2: Build**

Run: `cabal build postgresql-syntax`
Expected: succeeds (new module, nothing imports it yet).

- [ ] **Step 3: Commit**

```bash
git add library/PostgresqlSyntax/Ast/Internal.hs
git commit -m "Add PostgresqlSyntax.Ast.Internal for cross-node helper functions"
```

## Tasks 4-6: Extract the 40 singleton types

These have zero dependency on the giant SCC (verified by computation — no singleton's field references any of the 123 SCC members), so extraction order among themselves only needs to respect the dependency order below (each type only ever depends on types earlier in its own list). No `{-# SOURCE #-}` imports needed anywhere in these three tasks.

For every type in a batch: apply Pattern A/B/C from the "mechanical transformation pattern" section (identify which by checking whether `Ast.hs` declares it as `data`/`newtype` → Pattern A, or `type X = <primitive>` → Pattern B, or `type X = NonEmpty Y` → Pattern C — cross-reference the "Target module inventory" section above). Steps are the same shape for every type in the batch:

- [ ] Locate the type's `data`/`newtype`/`type` declaration in `Ast.hs` (search by name), its parser in `Parsing.hs` (same lowerCamel name), its renderer in `Rendering.hs` (same name), and its generator in `hedgehog-test/Main/Gen.hs` (same name).
- [ ] Create `library/PostgresqlSyntax/Ast/<Type>.hs` with the `data`/`newtype` decl + `instance IsAst` + `instance Arbitrary`, importing `PostgresqlSyntax.Ast.Internal` for any shared helper it uses and `PostgresqlSyntax.Ast.<Dep>` (real import — no dep here is a hub) for each other Ast type it references.
- [ ] Delete the type's declaration from `Ast.hs`; add `import PostgresqlSyntax.Ast.<Type>` to `Ast.hs`'s import list (temporary — Task 7 turns this into the proper re-export root) and add the type to a running `-- extracted, see Ast/<Type>.hs` marker comment block at the top so Task 7 can find everything that moved.
- [ ] Delete the parser function from `Parsing.hs`, delete the render function from `Rendering.hs`, delete the generator from `hedgehog-test/Main/Gen.hs`. If `Parsing.hs`/`Rendering.hs` used qualified access to this type's constructors elsewhere (grep the two files for the type's constructor names), update those call sites to `PostgresqlSyntax.Ast.<Type>.<Ctor>` or add an import.
- [ ] Build: `cabal build postgresql-syntax` — must succeed before moving to the next type in the batch.

**Task 4 — batch 1 (leaves with zero deps, 14 types):** `Ident, NameList` *(NameList depends on Name which is a bare alias to ColId to Ident — already resolvable once Ident exists)*, `Op, MathOp, AllOp, Iconst, ArrayBounds, AscDesc, Bconst, OptVarying, Character, ConstCharacter, Timezone, ConstDatetime`

- [ ] Apply the per-type steps above to each of the 14 types in order.
- [ ] Run the full existing test suite: `cabal test tasty-test hedgehog-test` — expected: PASS (nothing has changed behaviorally; if a test fails, the extraction introduced a bug — fix before continuing).
- [ ] Commit: `git add -A && git commit -m "Extract singleton AST nodes: batch 1 (Ident through ConstDatetime)"`

**Task 5 — batch 2 (13 types):** `Sconst, ExtractArg, Fconst, ForLockingStrength, FrameClauseMode, IntervalSecond, Interval, JoinType, NullsOrder, OptOrdinality, OverrideKind, AttrName` *(bare alias — no module; confirm it's left untouched)*, `Attrs`

Note: `AttrName` is listed in the bare-aliases set — skip it in the per-type steps (nothing to extract), but do extract `Attrs` (`NonEmpty AttrName`, Pattern C) right after, since `Attrs` needs `AttrName`'s target (`ColLabel` → `Ident`) resolvable, which it already is from Task 4.

- [ ] Apply the per-type steps to `Sconst, ExtractArg, Fconst, ForLockingStrength, FrameClauseMode, IntervalSecond, Interval, JoinType, NullsOrder, OptOrdinality, OverrideKind, Attrs` (12 real extractions).
- [ ] Run full test suite, fix if failing.
- [ ] Commit: `git add -A && git commit -m "Extract singleton AST nodes: batch 2 (Sconst through Attrs)"`

**Task 6 — batch 3 (13 types):** `AnyName, AnyOperator, QualAllOp, QualOp, SelectBinOp, SubType, SubqueryOp, SymbolicExprBinOp, TrimModifier, TypenameArrayDimensions, VerbalExprBinOp, WindowExclusionClause, Xconst, AliasClause`

- [ ] Apply the per-type steps to all 14 types (note: `AnyOperator` is self-recursive — `data AnyOperator = ... | AnyOperator AllOp AnyOperator` roughly; a same-file self-reference needs no special handling, just import itself is implicit).
- [ ] Run full test suite, fix if failing.
- [ ] Commit: `git add -A && git commit -m "Extract singleton AST nodes: batch 3 (AnyName through AliasClause) — all 40 singletons done"`
- [ ] Sanity check: `grep -c '^data \|^newtype \|^type ' library/PostgresqlSyntax/Ast.hs` should now report 173 − 40 = 133 remaining declarations (118 + 55 originally, minus the 40 just extracted — some of which were `type` aliases turned into modules, so the exact arithmetic is "40 fewer declarations physically present," verify by diffing against the "Target module inventory" list rather than expecting a round number).

## Task 7: `.hs-boot` skeletons for the 9 recursion hubs

**Files:**
- Create: `library/PostgresqlSyntax/Ast/AExpr.hs-boot`
- Create: `library/PostgresqlSyntax/Ast/ArrayExpr.hs-boot`
- Create: `library/PostgresqlSyntax/Ast/BExpr.hs-boot`
- Create: `library/PostgresqlSyntax/Ast/FuncExprCommonSubexpr.hs-boot`
- Create: `library/PostgresqlSyntax/Ast/SelectNoParens.hs-boot`
- Create: `library/PostgresqlSyntax/Ast/SelectWithParens.hs-boot`
- Create: `library/PostgresqlSyntax/Ast/SimpleSelect.hs-boot`
- Create: `library/PostgresqlSyntax/Ast/TableRef.hs-boot`
- Create: `library/PostgresqlSyntax/Ast/WithClause.hs-boot`

**Interfaces:**
- Produces: for each hub `X`, `data X` + `instance IsAst X` (abstract, no constructors, no method bodies) — every batch task from Task 8 onward that references a hub type imports it via `import {-# SOURCE #-} PostgresqlSyntax.Ast.<Hub>` until Task 16 creates the hub's real `.hs`.

- [ ] **Step 1: Write each boot file**, e.g. `Ast/AExpr.hs-boot`:

```haskell
module PostgresqlSyntax.Ast.AExpr where

import PostgresqlSyntax.IsAst (IsAst)

data AExpr

instance IsAst AExpr
```

Repeat verbatim (substituting the type name) for the other 8.

- [ ] **Step 2: Add all 9 to the cabal `other-modules`** — `.hs-boot` files are declared alongside their `.hs` counterpart implicitly by Cabal once both exist with matching names in the same `hs-source-dirs`; no separate cabal entry is needed for the `.hs-boot` itself, only for the eventual `.hs` (added per-hub in Task 16). Skip this step — no cabal change here.

- [ ] **Step 3: Build**

Run: `cabal build postgresql-syntax`
Expected: succeeds. (GHC tolerates an orphan `.hs-boot` with no corresponding `.hs` yet being present in the source tree as long as nothing imports it — these won't be imported until Task 8.)

- [ ] **Step 4: Commit**

```bash
git add library/PostgresqlSyntax/Ast/*.hs-boot
git commit -m "Add .hs-boot skeletons for the 9 AST recursion hubs"
```

## Tasks 8-15: Extract the 97 non-hub SCC types

Dependency-ordered list (each type depends only on types earlier in this list, the 40 already-extracted singletons, the 27 bare aliases in `Ast.hs`, or one of the 9 hubs via its `.hs-boot`):

```
ExprList, GenericType, Numeric, Bit, SimpleTypename, Typename, TypeList, InExpr, AExprReversableOp,
IndirectionEl, Indirection, FuncName, FuncArgExpr, SortBy, SortClause, FuncConstArgs, ConstTypename,
AexprConst, ArrayExprList, BExprIsOp, Columnref, WhenClause, WhenClauseList, CaseExpr,
FuncApplicationParams, FuncApplication, FrameBound, FrameExtent, FrameClause, WindowSpecification,
OverClause, FuncExpr, ExplicitRow, ImplicitRow, CExpr, CallStmt, TableFuncElement,
TableFuncElementList, SelectStmt, QualifiedName, InsertTarget, InsertColumnItem, InsertColumnList,
InsertRest, FuncExprWindowless, IndexElemDef, IndexElem, IndexParams, ConfExpr, SetTarget,
SetTargetList, SetClause, SetClauseList, OnConflictDo, OnConflict, TargetEl, TargetList, InsertStmt,
RelationExpr, RelationExprOptAlias, FromClause, WhereOrCurrentClause, UpdateStmt, FromList,
DeleteStmt, PreparableStmt, CommonTableExpr, ExtractList, ForLockingItem, ForLockingClause,
FuncAliasClause, RowsfromItem, RowsfromList, FuncTable, GroupByItem, GroupClause, OptTempTableName,
JoinQual, JoinMeth, JoinedTable, SelectLimitValue, SelectFetchFirstValue, LimitClause, OffsetClause,
OverlayList, PositionList, Row, SelectClause, SelectLimit, SubstrListFromFor, SubstrList,
TablesampleClause, Targeting, TrimList, ValuesClause, WindowDefinition, WindowClause
```

For any type in this list whose field references one of the 9 hubs (`AExpr, ArrayExpr, BExpr, FuncExprCommonSubexpr, SelectNoParens, SelectWithParens, SimpleSelect, TableRef, WithClause`), import that hub via `import {-# SOURCE #-} PostgresqlSyntax.Ast.<Hub> (<Hub>)`. Cross-reference the "Direct Ast-type refs" against the hub names to know which of a type's imports need `SOURCE`. Otherwise, the per-type steps are identical to Tasks 4-6 (apply Pattern A/B/C/D/E as appropriate — `SelectStmt` and `SelectClause` are Pattern D; `ExplicitRow` is Pattern E; everything else here is Pattern A or C).

Split into 8 batches of ~12, each ending in a full test-suite run and a commit:

- [ ] **Task 8 — batch 1:** `ExprList, GenericType, Numeric, Bit, SimpleTypename, Typename, TypeList, InExpr, AExprReversableOp, IndirectionEl, Indirection, FuncName`. Test + commit (`"Extract SCC AST nodes: batch 1 (types)"`).
- [ ] **Task 9 — batch 2:** `FuncArgExpr, SortBy, SortClause, FuncConstArgs, ConstTypename, AexprConst, ArrayExprList, BExprIsOp, Columnref, WhenClause, WhenClauseList, CaseExpr`. Test + commit (`"...batch 2 (operators, constants, case)"`).
- [ ] **Task 10 — batch 3:** `FuncApplicationParams, FuncApplication, FrameBound, FrameExtent, FrameClause, WindowSpecification, OverClause, FuncExpr, ExplicitRow, ImplicitRow, CExpr, CallStmt`. Test + commit (`"...batch 3 (func application, window, CExpr)"`).
- [ ] **Task 11 — batch 4:** `TableFuncElement, TableFuncElementList, SelectStmt, QualifiedName, InsertTarget, InsertColumnItem, InsertColumnList, InsertRest, FuncExprWindowless, IndexElemDef, IndexElem, IndexParams`. Test + commit (`"...batch 4 (insert, index)"`).
- [ ] **Task 12 — batch 5:** `ConfExpr, SetTarget, SetTargetList, SetClause, SetClauseList, OnConflictDo, OnConflict, TargetEl, TargetList, InsertStmt, RelationExpr, RelationExprOptAlias`. Test + commit (`"...batch 5 (conflict, set, target, insert stmt)"`).
- [ ] **Task 13 — batch 6:** `FromClause, WhereOrCurrentClause, UpdateStmt, FromList, DeleteStmt, PreparableStmt, CommonTableExpr, ExtractList, ForLockingItem, ForLockingClause, FuncAliasClause, RowsfromItem`. Test + commit (`"...batch 6 (update, delete, CTE)"`).
- [ ] **Task 14 — batch 7:** `RowsfromList, FuncTable, GroupByItem, GroupClause, OptTempTableName, JoinQual, JoinMeth, JoinedTable, SelectLimitValue, SelectFetchFirstValue, LimitClause, OffsetClause`. Test + commit (`"...batch 7 (func table, join, limit)"`).
- [ ] **Task 15 — batch 8 (final SCC batch, 13 types):** `OverlayList, PositionList, Row, SelectClause, SelectLimit, SubstrListFromFor, SubstrList, TablesampleClause, Targeting, TrimList, ValuesClause, WindowDefinition, WindowClause`. Test + commit (`"...batch 8 (overlay, row, select clause, window) — all 97 non-hub SCC nodes done"`).
  - [ ] Additionally: `hedgehog-test/Main/Gen.hs` should now be empty except for genuinely shared generator infrastructure (`listElement`, size/range helpers used by 2+ types). Move those into `library/PostgresqlSyntax/Ast/Internal.hs` (or a new `Ast/Internal/Arbitrary.hs` if `Internal.hs` is getting large) alongside the parsing/rendering helpers from Task 3, since they're needed by node modules' `Arbitrary` instances, not by test code.

## Task 16: Extract the 9 recursion hubs (real modules)

**Files:**
- Create: `library/PostgresqlSyntax/Ast/AExpr.hs`, `.../ArrayExpr.hs`, `.../BExpr.hs`, `.../FuncExprCommonSubexpr.hs`, `.../SelectNoParens.hs`, `.../SelectWithParens.hs`, `.../SimpleSelect.hs`, `.../TableRef.hs`, `.../WithClause.hs`
- Modify: `library/PostgresqlSyntax/Ast.hs` (delete these 9 remaining declarations)
- Modify: `postgresql-syntax.cabal` (add all 9 `.hs` to `other-modules` — the matching `.hs-boot` files from Task 7 are then automatically paired by Cabal/GHC)

**Interfaces:**
- Consumes: every one of the other 137 module-producing types (all real modules by now), plus the 9 `.hs-boot` files from Task 7.
- Produces: the 9 hubs as real modules. Any already-extracted module that has a genuine mutual cycle with one of these 9 (identified during Tasks 4-15 by needing the `SOURCE` import) keeps that `SOURCE` import permanently — do not "clean it up" to a real import; that would reintroduce the cycle GHC just told you doesn't compile.

At this point `Ast.hs` should contain nothing but the 27 bare `type` aliases and these 9 hub declarations. Extract each hub:

- [ ] For each of the 9 hubs (any order — since every non-hub dependency is already real, and cross-hub dependencies are exactly the ones the `.hs-boot` files paper over): create `Ast/<Hub>.hs` per Pattern A/F, using a real import for every non-hub dependency and `import {-# SOURCE #-} PostgresqlSyntax.Ast.<OtherHub>` for any of the other 8 it references (check each hub's "Direct Ast-type refs" from the inventory against the 9-hub set).
- [ ] Delete each hub's declaration from `Ast.hs`, delete its parser from `Parsing.hs`, its renderer from `Rendering.hs`, its generator from `hedgehog-test/Main/Gen.hs`.
- [ ] Add all 9 `.hs` files to the cabal `other-modules`.
- [ ] Build: `cabal build postgresql-syntax`. This is the highest-risk build in the whole plan (9 mutually-recursive modules coming online simultaneously with a mix of `SOURCE` and real imports) — if GHC reports "cannot find module" or a boot-file mismatch, check: (a) the `.hs-boot`'s `instance IsAst X` line matches exactly what the real `.hs` declares (same class, same type), (b) every module with a genuine cycle back into a hub uses `SOURCE`, not a real import, (c) the hub's own `.hs` does NOT import its own `.hs-boot`.
- [ ] Run full test suite: `cabal test tasty-test hedgehog-test`. Expected: PASS.
- [ ] Commit:

```bash
git add -A
git commit -m "Extract the 9 AST recursion hubs into real modules"
```

## Task 17: Finish the `Ast.hs` root module

**Files:**
- Modify: `library/PostgresqlSyntax/Ast.hs`

**Interfaces:**
- Produces: `PostgresqlSyntax.Ast` as a pure re-export root (Aggregator Namespace Variant 2) — every external caller and every remaining in-tree module imports this instead of any `Ast.<Type>` submodule directly (submodules are hidden, per Task 18's cabal split).

- [ ] **Step 1: Rewrite `Ast.hs`** to just the 27 bare aliases plus a full re-export list:

```haskell
module PostgresqlSyntax.Ast
  ( module PostgresqlSyntax.Ast.Ident,
    module PostgresqlSyntax.Ast.NameList,
    -- ... all 146 module-producing types, one `module` line each, alphabetical
    ColId,
    ColLabel,
    Name,
    -- ... all 27 bare aliases, listed as plain type exports (not `module` re-exports, since they have no module)
  )
where

import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.NameList
-- ... all 146, alphabetical

type ColId = Ident

type ColLabel = Ident

-- ... remaining 25 bare aliases, unchanged from their original text in the pre-migration Ast.hs
```

- [ ] **Step 2: Build**

Run: `cabal build postgresql-syntax`
Expected: succeeds. Check for ambiguous-export warnings (two submodules exporting the same name) — none expected, since Variant 2's rule ("all exported names MUST be globally unambiguous") was upheld by every constructor/field naming choice in Tasks 4-16, but verify.

- [ ] **Step 3: Full test suite**

Run: `cabal test tasty-test hedgehog-test`
Expected: PASS — this is the first point since Task 3 where the *entire* AST module surface has moved; a green run here confirms the whole split is behavior-preserving.

- [ ] **Step 4: Commit**

```bash
git add library/PostgresqlSyntax/Ast.hs
git commit -m "Finish PostgresqlSyntax.Ast as a pure re-export root"
```

## Task 18: `IsAst` becomes the `PostgresqlSyntax` surface module; delete `Parsing.hs`/`Rendering.hs`

**Files:**
- Create: `library/PostgresqlSyntax.hs`
- Modify: `library/PostgresqlSyntax/IsAst.hs` (generalize `run`/`runWithPosError`/`atEnd`/`toText`)
- Delete: `library/PostgresqlSyntax/Parsing.hs`
- Delete: `library/PostgresqlSyntax/Rendering.hs`
- Modify: `postgresql-syntax.cabal` (remove `PostgresqlSyntax.Parsing`/`.Rendering` from `exposed-modules`, add `PostgresqlSyntax` and keep `PostgresqlSyntax.IsAst`)

By this point `Parsing.hs` and `Rendering.hs` should contain nothing but the generic, non-per-type helpers already copied into `Ast/Internal.hs` by Task 3 (`run`, `runWithPosError`, `atEnd`, and the combinators) — every per-type function was deleted during Tasks 4-16. Confirm this with `grep -c '^[a-zA-Z]' library/PostgresqlSyntax/Parsing.hs library/PostgresqlSyntax/Rendering.hs` before deleting; if anything unexpected remains, it's a type that got missed in an earlier batch — go extract it first.

- [ ] **Step 1: Move the 4 generic executor functions into `IsAst.hs`, generalized**

```haskell
-- IsAst.hs, additions
run :: (IsAst a) => Text -> Either String a
run = Extras.run parser

runWithPosError :: (IsAst a) => Text -> Either (NonEmpty (Int, String)) a
runWithPosError = Extras.runParserWithErrorPos parser

atEnd :: Parser a -> Parser a
atEnd p = space *> p <* endHead <* space <* eof

toText :: (IsAst a) => a -> Text
toText = TextBuilder.toText . toTextBuilder
```

(`run`/`runWithPosError` change shape slightly from the original `Parser a -> Text -> Either ... a` to `Text -> Either ... a` since the parser is now always `parser` for whatever `IsAst` instance the caller's type-inference picks — this is the "generalized over `IsAst a =>`" the design doc calls for. If a caller needs to run a *non-default* parser for a type, that's not a case that exists in the current codebase — verified in Task 3 that `run`/`runWithPosError` were always called with a type's own top-level parser, never a sub-parser.)

- [ ] **Step 2: Create `library/PostgresqlSyntax.hs`**

```haskell
module PostgresqlSyntax
  ( module PostgresqlSyntax.IsAst,
  )
where

import PostgresqlSyntax.IsAst
```

- [ ] **Step 3: Delete `Parsing.hs` and `Rendering.hs`**

```bash
git rm library/PostgresqlSyntax/Parsing.hs library/PostgresqlSyntax/Rendering.hs
```

- [ ] **Step 4: Update the cabal `library` stanza**

Remove `PostgresqlSyntax.Parsing` and `PostgresqlSyntax.Rendering` from `exposed-modules`. Add `PostgresqlSyntax` to `exposed-modules`. `PostgresqlSyntax.IsAst` moves from `other-modules` to `exposed-modules` (it's public now, re-exported through `PostgresqlSyntax`).

- [ ] **Step 5: Fix remaining references to `Parsing.X`/`Rendering.X`**

`tasty-test/Main.hs` and `hedgehog-test/Main.hs` still call `Parsing.preparableStmt`, `Rendering.aExpr`, etc. — Task 19 replaces both test suites wholesale, so for this task just get the *library* building; it's fine (and expected) for the two test-suite components to be red until Task 19. Do not attempt to patch them here.

- [ ] **Step 6: Build the library only**

Run: `cabal build postgresql-syntax` (the library component, not the test-suites)
Expected: succeeds.

- [ ] **Step 7: Commit**

```bash
git add -A
git commit -m "Replace Parsing/Rendering with generalized IsAst-based PostgresqlSyntax surface module"
```

## Task 19: Replace `tasty-test`/`hedgehog-test` with `hspec-test`

**Files:**
- Create: `hspec-test/Main.hs`
- Create: `hspec-test/Spec.hs` (the `hspec-discover` driver)
- Delete: `tasty-test/`
- Delete: `hedgehog-test/`
- Modify: `postgresql-syntax.cabal` (remove `tasty-test`/`hedgehog-test` test-suites, add `hspec-test`)

**Interfaces:**
- Consumes: `PostgresqlSyntax` (`run`, `runWithPosError`, `toText`), `PostgresqlSyntax.Ast` (all types + their `Arbitrary`/`IsAst` instances).
- Produces: nothing consumed elsewhere — this is the leaf test-suite component.

- [ ] **Step 1: Port the round-trip properties**

`hedgehog-test/Main.hs` (36 lines) had 4 `Hspec`-worthy round-trip properties: generate via `Arbitrary`, render via `toText`, re-parse via `run`, assert equality. Port each using `Test.Hspec.QuickCheck.prop`:

```haskell
-- hspec-test/Main.hs
module Main (main) where

import PostgresqlSyntax
import PostgresqlSyntax.Ast
import PostgresqlSyntax.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

roundTrip :: (IsAst a, Eq a, Show a) => a -> Bool
roundTrip a = run (toText a) == Right a

main :: IO ()
main = hspec do
  describe "Round-trip parse/render" do
    prop "Typename" (roundTrip @Typename)
    prop "TableRef" (roundTrip @TableRef)
    prop "AExpr" (roundTrip @AExpr)
    prop "PreparableStmt" (roundTrip @PreparableStmt)
```

- [ ] **Step 2: Port the example-based tests**

`tasty-test/Main.hs` (157 lines) had hand-picked SQL snippets (`Parsing.preparableStmt`, `Parsing.typename`, `Parsing.sconst`/JSONB inputs) and error-message regression tests (`Parsing.runWithPosError`), plus `nestingTests` (deep-parenthesis regressions via `Parsing.aExpr`/`.preparableStmt`/`.selectNoParens`/`.selectWithParens`/`.selectWithParensBody`, `Rendering.aExpr`). Read the current file's exact snippets/expected outputs (they don't change — only the call sites do: `Parsing.preparableStmt` → `run @PreparableStmt`, `Rendering.aExpr` → `toTextBuilder`/`toText`) and port each `testCase`/`testGroup` to an `it`/`describe` block in the same `hspec-test/Main.hs`, or a second `hspec-test/ExamplesSpec.hs` if `Main.hs` gets unwieldy (`hspec-discover` picks up any `*Spec.hs` automatically).

- [ ] **Step 3: Add the cabal test-suite**

```
test-suite hspec-test
  import: base-settings
  type: exitcode-stdio-1.0
  hs-source-dirs: hspec-test
  main-is: Spec.hs
  build-tool-depends: hspec-discover:hspec-discover
  ghc-options: -threaded
  other-modules: Main
  build-depends:
    hspec >=2.10 && <3,
    postgresql-syntax,
    rerebase <2,
```

(`Spec.hs` is the 2-line `hspec-discover` boilerplate: `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}`.)

- [ ] **Step 4: Remove the old test-suites**

```bash
git rm -r tasty-test hedgehog-test
```

Remove their stanzas from `postgresql-syntax.cabal`, and update `keyword-bench`'s `hs-source-dirs`/`other-modules` (it currently reuses `hedgehog-test`'s `Main.Gen` for input generation) — point it at the new colocated `Arbitrary` instances instead: `keyword-bench/Main.hs` generates via `PostgresqlSyntax.Ast`'s `arbitrary` (QuickCheck) rather than `SynGen`/Hedgehog; update its imports and its `sample`/`generate`-style call accordingly (QuickCheck's `generate arbitrary` in place of Hedgehog's `Gen.sample`).

- [ ] **Step 5: Build and test**

Run: `cabal test`
Expected: `hspec-test` and both benchmarks build; `hspec-test` passes.

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "Replace tasty-test/hedgehog-test with a single hspec test-suite"
```

## Task 20: Split into internal + public library

**Files:**
- Modify: `postgresql-syntax.cabal`

**Interfaces:**
- Produces: `postgresql-syntax-internal` (everything) + public `postgresql-syntax` (`PostgresqlSyntax.Ast`, `PostgresqlSyntax` only, via `reexported-modules`).

- [ ] **Step 1: Rename the current `library` stanza to `postgresql-syntax-internal`**

```
library postgresql-syntax-internal
  import: base-settings
  hs-source-dirs: library
  exposed-modules:
    PostgresqlSyntax
    PostgresqlSyntax.Ast
    PostgresqlSyntax.Ast.<every node module, alphabetical>
    PostgresqlSyntax.IsAst
    PostgresqlSyntax.KeywordSet
    PostgresqlSyntax.Validation

  other-modules:
    PostgresqlSyntax.Ast.Internal
    PostgresqlSyntax.CharSet
    PostgresqlSyntax.Extras.HeadedMegaparsec
    PostgresqlSyntax.Extras.NonEmpty
    PostgresqlSyntax.Extras.TextBuilder
    PostgresqlSyntax.Predicate
    PostgresqlSyntax.Prelude

  build-depends:
    QuickCheck >=2.14 && <3,
    base >=4.12 && <5,
    bytestring >=0.10 && <0.13,
    case-insensitive >=1.2.1 && <2,
    hashable >=1.3.5 && <2,
    headed-megaparsec >=0.2.0.1 && <0.3,
    megaparsec >=9.2 && <10,
    parser-combinators >=1.3 && <1.4,
    text >=1 && <3,
    text-builder >=1 && <1.1,
    unordered-containers >=0.2.16 && <0.3,
```

(All node modules are `exposed-modules` here, not `other-modules` — `postgresql-syntax-internal` is only visible to sibling components within this package per Cabal's `--internal-library` scoping, so "exposed" here just means "importable by other stanzas in this file," not "public on Hackage.")

- [ ] **Step 2: Add the public `library` stanza**

```
library
  import: base-settings
  reexported-modules:
    PostgresqlSyntax.Ast as PostgresqlSyntax.Ast,
    PostgresqlSyntax as PostgresqlSyntax,

  build-depends:
    postgresql-syntax-internal,
```

- [ ] **Step 3: Update all in-tree components' `build-depends`**

`hspec-test`, `keyword-bench`, and `nesting-bench` all need direct access to hidden internal modules (`KeywordSet` for `keyword-bench`; `Arbitrary` instances live on the public `Ast` types so `hspec-test` is fine against the public library, but check — if any of its ported example tests reached into `Validation`/`KeywordSet` directly, they need the internal dependency too). Change `build-depends: postgresql-syntax` to `build-depends: postgresql-syntax-internal` wherever a component imports anything other than `PostgresqlSyntax`/`PostgresqlSyntax.Ast`.

- [ ] **Step 4: Build everything**

Run: `cabal build all`
Expected: succeeds — this is the point where an accidental hidden-module import from a public-library-only component would surface as "module not found."

- [ ] **Step 5: Full test run**

Run: `cabal test`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "Split into postgresql-syntax-internal + public postgresql-syntax library"
```

## Task 21: Version bump + CHANGELOG

**Files:**
- Modify: `postgresql-syntax.cabal` (`version:`)
- Modify: `CHANGELOG.md`

- [ ] **Step 1: Bump the version**

Change `version: 0.4.5.0` to `version: 0.5.0.0` (major bump — breaking change per PVP, since `PostgresqlSyntax.Parsing`/`.Rendering`/`.KeywordSet`/`.Validation` are no longer importable and `PostgresqlSyntax.Ast`'s internal shape changed for anyone who pattern-matched former `Either`/`Maybe`/bare-`Text` aliases).

- [ ] **Step 2: Add the CHANGELOG entry**

Follow the existing `CHANGELOG.md` format. Under a new `## 0.5.0.0` heading, a `### Breaking` section listing: `PostgresqlSyntax.Parsing` and `.Rendering` removed (replaced by `PostgresqlSyntax`'s generalized `run`/`runWithPosError`/`toText`/`atEnd` + the `IsAst` class); `PostgresqlSyntax.KeywordSet`/`.Validation` no longer part of the public API surface; several former type aliases (`SelectStmt`, `SelectClause`, `ExplicitRow`, and the 10 primitive-wrapper types listed in "Target module inventory") are now distinct ADTs/newtypes instead of bare `Either`/`Maybe`/`Text`/`Int64`/`Double`/`Bool` aliases.

- [ ] **Step 3: Commit**

```bash
git add postgresql-syntax.cabal CHANGELOG.md
git commit -m "Bump to 0.5.0.0 for the AST node module restructuring"
```

## Task 22: Final verification

- [ ] Run: `cabal build all` — expected: clean build, no warnings about unused imports left over from the migration (`-Wunused-imports` if enabled; otherwise manually grep each touched file for now-dead `import qualified PostgresqlSyntax.KeywordSet as KeywordSet`-style leftovers in modules that no longer use them).
- [ ] Run: `cabal test` — expected: `hspec-test` passes in full.
- [ ] Run: `cabal bench keyword-bench nesting-bench` — expected: both run to completion (performance regression is out of scope to fix, per the design doc's "Out of scope" section, but they must at least execute).
- [ ] Run: `grep -rn 'PostgresqlSyntax.Parsing\|PostgresqlSyntax.Rendering' .` (excluding `.git`, `dist-newstyle`, `CHANGELOG.md`) — expected: zero hits; confirms no stray reference to the deleted modules survived anywhere in the tree.
- [ ] Confirm `library/PostgresqlSyntax/Ast.hs` contains exactly the 27 bare aliases + the full re-export list, no stray `data`/`newtype` declarations.
