# Postgres Grammar Sync: Postfix `qual_Op` Removal Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Bring the expression grammar back in sync with the pinned Postgres `gram.y` by deleting the `a_expr qual_Op` / `b_expr qual_Op` postfix-operator productions (removed from Postgres in v14), then re-derive the `filteredParser` keyword-exclusion sites against the reference grammar's actual follow sets.

**Architecture:** Three layers of the current code work around a production Postgres no longer has. `SuffixQualOpAExpr` renders as `<operand> <op>` with no right operand, so on reparse `suffixRec`'s loop re-enters `symbolicBinOpExpr` and swallows whatever follows as the operator's right operand. `safeAExprOperand` (generator-side) hides the resulting counterexamples; `AExpr.filteredParser` (parser-side) blocks specific words from being swallowed at specific call sites. Deleting the production removes the cause; the two workarounds then shrink to only the responsibilities they genuinely have (operator-precedence parenthesization for `safeAExprOperand`, real grammatical follow sets for `filteredParser`).

**Tech Stack:** GHC 9.12.4 / Cabal, `headed-megaparsec`, `text-builder`, QuickCheck + hspec (`hspec-test`), hedgehog (`hedgehog-test`).

## Global Constraints

- The reference grammar is pinned in `AGENTS.md` to Postgres commit `c12c101b0846b1e6488f2dc986a852fbc6bf2e3b`, vendored at `references/gram.y`, `references/kwlist.h`, `references/scan.l`. Every grammar claim in this plan cites a line in that vendored file. Do not consult a different Postgres version.
- Follow `docs/conventions.md` (project-local overrides of nikita-volkov/haskell-coding-standards): `Test.QuickCheck` qualified as `Qc`; `HeadedMegaparsec` *and* `PostgresqlSyntax.Extras.HeadedMegaparsec` both aliased to `Parser`.
- Follow `AGENTS.md`'s `Arbitrary` rules: `Gens.downscale` applies **only** to self-recursive fields and to types imported via `import {-# SOURCE #-}`; nothing else gets downscaled.
- This is a breaking change to a published Hackage package (0.4.5.0). A `CHANGELOG.md` entry under the existing `# Upcoming` → `## Breaking` section is required (Task 6).
- Every task ends with a green `cabal build` and a `cabal test hspec-test --enable-tests` run whose failure set is **no worse than** the baseline recorded in Task 1.
- Never claim a task is done without pasting the actual command output. `cabal test` exits non-zero on failure; read the count, don't assume.

## Baseline (measured 2026-07-29 on `ast-node-modules-restructure` @ `84d0c25`)

`cabal test hspec-test --enable-tests --test-options="--seed 1803371098"` → **431 examples, 19 failures**, all of them `Properties.<Type>.IsAst Roundtrips`. Classified by which constructor appears in the shrunk counterexample:

| Cluster | Failing properties | In scope? |
|---|---|---|
| `SuffixQualOpAExpr` (postfix `qual_Op`) | ExplicitRow, InExpr, OverClause, OverlayList, SubstrList, TablesampleClause, TrimList, WindowDefinition — **8** | **Yes — this plan** |
| `Op "?"` colliding with the library's non-Postgres `?` typename-nullability extension, without a postfix op | ForLockingItem, SelectClause — 2 | No (follow-up) |
| `SubqueryAExpr` (`a_expr subquery_Op sub_type …`) | CallStmt, DeleteStmt, FuncApplication, SimpleSelect, UpdateStmt, WhenClause, WithClause — 7 | No (follow-up) |
| `AllIndirectionEl` (`.*`) reaching positions that reject it | JoinedTable — 1 | No (follow-up) |
| Unclassified | PreparableStmt — 1 | No (follow-up) |

(Clusters overlap: several counterexamples carry two markers. The failure *count* varies with the QuickCheck seed — always pass `--seed 1803371098` when comparing against this baseline.)

**Success criterion for this plan:** the 8 `SuffixQualOpAExpr` properties go green and stay green; the other 11 are untouched and explicitly out of scope. Do not attempt them here — they are separate root causes and belong in their own plans.

## Why this is a sync fix, not a workaround

`references/gram.y` has exactly two `qual_Op` expression productions:

```
15985:            | a_expr qual_Op a_expr             %prec Op
15987:            | qual_Op a_expr                    %prec Op
```

and for `b_expr`:

```
16472:            | b_expr qual_Op b_expr             %prec Op
16474:            | qual_Op b_expr                    %prec Op
```

There is **no** `a_expr qual_Op` and **no** `b_expr qual_Op`. Postfix operators were removed from Postgres in v14; the precedence preamble refers to this in passing at `references/gram.y:903-904` ("not really necessary since we removed postfix operators"). `x OPERATOR(pg_catalog.+#)` is a syntax error in modern Postgres, so no correct parser needs to accept it, and no correct renderer needs to emit it.

Note `BExpr` already has **no** constructor for the postfix form (`library/PostgresqlSyntax/Ast/BExpr.hs:51-58` — the reference comment at line 39 lists `b_expr qual_Op` but the data type never implemented it). Only `AExpr` has it.

## File Structure

- `library/PostgresqlSyntax/Ast/AExpr.hs` — remove the `SuffixQualOpAExpr` constructor and its renderer / parser / `isBoundedAExprOperand` / `Arbitrary` cases; correct the `a_expr` reference comment; rewrite the `isBoundedAExprOperand` doc paragraph that is entirely about this constructor.
- `library/PostgresqlSyntax/Ast/BExpr.hs` — correct the `b_expr` reference comment only (no code change).
- `library/PostgresqlSyntax/Ast/TargetEl.hs` — the `ImplicitlyAliasedExprTargetEl` comment names `SuffixQualOpAExpr` as its motivating example; re-word.
- `library/PostgresqlSyntax/Ast/FrameBound.hs`, `.../SortBy.hs`, `.../IndexElem.hs` — audit the `filteredParser` exclusion lists against the grammar's real follow sets (Task 4).
- `library/PostgresqlSyntax/Ast/AExpr.hs-boot` — only if an exported name disappears (it does not; `filteredParser`, `isBoundedAExprOperand`, `safeAExprOperand`, `selectWithParensAExpr` all survive).
- `hedgehog-test/Main/Gen.hs` — two `SuffixQualOpAExpr` generator lines (449, 500).
- `hspec-test/Cases.hs` — new example-based regression tests and one new helper.
- `library-internal/PostgresqlSyntax/KeywordSet.hs` — keyword sync (Task 5), via merging the existing `update-keywords` branch.
- `CHANGELOG.md` — breaking entry (Task 6).

---

### Task 1: Record the baseline and add the failing regression cases

**Files:**
- Modify: `hspec-test/Cases.hs` (add a `describe` group inside `spec`, and one helper in the `-- * Example-based parse helpers` section at the bottom)

**Interfaces:**
- Produces: `rejects :: forall a. (HasCallStack, IsAst a, Show a) => Text -> Expectation` in `Cases.hs`, used by Tasks 1 and 4.

- [ ] **Step 1: Record the baseline into the task log**

Run:

```bash
cabal test hspec-test --enable-tests --test-options="--seed 1803371098" 2>&1 | tail -5
```

Expected (approximately — record the exact numbers you see):

```
431 examples, 19 failures
Test suite hspec-test: FAIL
```

Paste the output into your progress notes. Every later task compares against it.

- [ ] **Step 2: Add the `rejects` helper**

In `hspec-test/Cases.hs`, under the `-- * Example-based parse helpers` heading, next to the existing `parsesTo`, add:

```haskell
-- | Asserts that the input is *not* accepted. Used to pin grammar
-- constructs that Postgres itself rejects.
rejects :: forall a. (HasCallStack, IsAst a, Show a) => Text -> Expectation
rejects input =
  case parse @a input of
    Left _ -> pure ()
    Right a ->
      expectationFailure
        ("expected a parse failure\ninput: " <> Text.unpack input <> "\nparsed: " <> show a)
```

- [ ] **Step 3: Add the failing tests**

In `hspec-test/Cases.hs`, inside `spec`, after the existing `describe "Parsers" $ do … ` block and before `describe "Error reporting"`, add:

```haskell
  -- Grammar constructs pinned directly against
  -- @references/gram.y@ at the commit recorded in AGENTS.md.
  describe "Postgres grammar conformance" $ do
    -- gram.y:15985,15987 have only @a_expr qual_Op a_expr@ and
    -- @qual_Op a_expr@ — the postfix @a_expr qual_Op@ form was removed
    -- from Postgres in v14.
    it "rejects postfix operators" $ do
      rejects @AExpr "1 +#"
      rejects @AExpr "1 OPERATOR(pg_catalog.+#)"
      rejects @AExpr "a +#"

    -- gram.y:17567 frame_bound. UNBOUNDED is an unreserved keyword, so
    -- @UNBOUNDED PRECEDING@ is ambiguous with @a_expr PRECEDING@ where
    -- the a_expr is a column named "unbounded"; gram.y:915 resolves it by
    -- giving UNBOUNDED lower precedence than PRECEDING, i.e. the keyword
    -- reading wins and the column reading needs quoting.
    it "frame_bound" $ do
      let render :: FrameBound -> Text
          render = toText
      fmap render (parse @FrameBound "unbounded preceding") `shouldBe` Right "UNBOUNDED PRECEDING"
      fmap render (parse @FrameBound "unbounded following") `shouldBe` Right "UNBOUNDED FOLLOWING"
      fmap render (parse @FrameBound "current row") `shouldBe` Right "CURRENT ROW"
      fmap render (parse @FrameBound "1 preceding") `shouldBe` Right "1 PRECEDING"
      fmap render (parse @FrameBound "a following") `shouldBe` Right "a FOLLOWING"
      fmap render (parse @FrameBound "\"unbounded\" preceding") `shouldBe` Right "\"unbounded\" PRECEDING"

    -- gram.y:17428 window_specification: the sort clause and the
    -- partition clause are both followed by opt_frame_clause, whose
    -- leading keywords (RANGE/ROWS/GROUPS, kwlist.h:375,408,201) are
    -- unreserved and therefore also legal ColIds.
    it "window_specification terminators are not swallowed by the expression" $ do
      parsesTo @WindowSpecification "(order by a rows unbounded preceding)"
      parsesTo @WindowSpecification "(order by a range unbounded preceding)"
      parsesTo @WindowSpecification "(partition by a groups unbounded preceding)"
      parsesTo @WindowSpecification "(partition by a order by b rows 1 preceding)"
```

- [ ] **Step 4: Run the new tests and confirm the postfix ones fail**

Run:

```bash
cabal test hspec-test --enable-tests --test-options='--match "/Cases/Postgres grammar conformance/"' 2>&1 | tail -30
```

Expected: `rejects postfix operators` FAILS with `expected a parse failure … parsed: SuffixQualOpAExpr …`. The other two `it` blocks are expected to pass already — if either fails, record which assertion and keep going; Tasks 2 and 4 must leave them green.

- [ ] **Step 5: Commit**

```bash
git add hspec-test/Cases.hs
git commit -m "Add grammar-conformance cases for postfix operators and frame bounds"
```

---

### Task 2: Delete the postfix `qual_Op` production from `AExpr`

**Files:**
- Modify: `library/PostgresqlSyntax/Ast/AExpr.hs` (lines 55, 109, 133, 241, 337-346, 353, 406 as of `84d0c25`)
- Modify: `library/PostgresqlSyntax/Ast/BExpr.hs:39`
- Modify: `library/PostgresqlSyntax/Ast/TargetEl.hs:71-78`
- Modify: `hedgehog-test/Main/Gen.hs:449,500`

**Interfaces:**
- Consumes: `rejects` from Task 1.
- Produces: `AExpr` without the `SuffixQualOpAExpr` constructor. `filteredParser`, `isBoundedAExprOperand`, `safeAExprOperand` and `selectWithParensAExpr` keep their existing signatures, so `AExpr.hs-boot` needs no change.

- [ ] **Step 1: Remove the constructor from the reference comment**

In `library/PostgresqlSyntax/Ast/AExpr.hs`, delete this line from the `a_expr` reference block:

```haskell
--   | a_expr qual_Op
```

- [ ] **Step 2: Remove the constructor**

Delete from the `data AExpr` declaration:

```haskell
  | SuffixQualOpAExpr AExpr QualOp
```

`QualOp` stays imported — `PrefixQualOpAExpr QualOp AExpr` still uses it.

- [ ] **Step 3: Remove the renderer case**

Delete from `toTextBuilder`:

```haskell
    SuffixQualOpAExpr a b -> renderOperand a <> " " <> toTextBuilder b
```

- [ ] **Step 4: Remove the parser alternative**

Delete from `customizedParser`'s `suffix` list (it sits directly after the `Parsers.symbolicBinOpExpr` line):

```haskell
          SuffixQualOpAExpr a <$> (Parsers.space *> parser),
```

- [ ] **Step 5: Remove the generator case**

Delete from `instance Qc.Arbitrary AExpr`'s `Qc.oneof` list:

```haskell
              SuffixQualOpAExpr <$> safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Qc.arbitrary,
```

- [ ] **Step 6: Remove the `isBoundedAExprOperand` case and rewrite its doc**

Delete this line from `isBoundedAExprOperand`:

```haskell
  SuffixQualOpAExpr {} -> False
```

Then replace the whole second paragraph of the Haddock above `isBoundedAExprOperand` — the one beginning `-- 'SuffixQualOpAExpr' fails that second half:` and ending `-- leftover, unparseable @TIME ZONE y@.` — with:

```haskell
-- Note this predicate is purely about operator precedence and
-- associativity: an unbounded shape is one whose own rendering ends in an
-- unrestricted recursive @a_expr@, so placed bare in the left position it
-- would re-absorb the suffix that follows (e.g. rendering
-- @SymbolicBinOpAExpr (NotAExpr x) op y@ plainly as @NOT x op y@ reparses
-- as @NotAExpr (SymbolicBinOpAExpr x op y)@). It is *not* a guard against
-- an expression swallowing a keyword that terminates an enclosing
-- production — the only shape that ever created that hazard was the
-- postfix @a_expr qual_Op@ production, which no longer exists here or in
-- @references/gram.y@ (see gram.y:15985,15987; Postgres removed postfix
-- operators in v14).
```

Also delete the ` __and__ nothing that could` … ` mistaken for the start of a fresh operand.` clause from the *first* paragraph, so it reads:

```haskell
-- position of a suffix production without parenthesizing it — see
-- @renderOperand@ in the 'IsAst' instance above for why that position is
-- special. A shape is bounded when parsing it can never end in an
-- unrestricted recursive @a_expr@ call, i.e. control is guaranteed to
-- return to @suffixRec@'s loop once it's done.
```

- [ ] **Step 7: Fix the `BExpr` reference comment**

In `library/PostgresqlSyntax/Ast/BExpr.hs`, delete this line from the `b_expr` reference block (the type never had a constructor for it; see gram.y:16472,16474):

```haskell
--   | b_expr qual_Op
```

- [ ] **Step 8: Re-word the `TargetEl` comment**

In `library/PostgresqlSyntax/Ast/TargetEl.hs`, replace the two lines

```haskell
        -- 'PostgresqlSyntax.Ast.AExpr.isBoundedAExprOperand' guards
        -- against (e.g. a trailing 'PostgresqlSyntax.Ast.AExpr.SuffixQualOpAExpr'
        -- would otherwise swallow the alias as its own operand instead).
```

with

```haskell
        -- 'PostgresqlSyntax.Ast.AExpr.isBoundedAExprOperand' guards
        -- against (e.g. rendering an 'PostgresqlSyntax.Ast.AExpr.OrAExpr'
        -- bare here would let its right operand absorb the alias).
```

- [ ] **Step 9: Remove the hedgehog generator cases**

In `hedgehog-test/Main/Gen.hs`, delete **both** occurrences (currently lines 449 and 500) of:

```haskell
      SuffixQualOpAExpr <$> prefixAExpr <*> qualOp,
```

- [ ] **Step 10: Build**

Run:

```bash
cabal build all --enable-tests
```

Expected: no errors. If GHC reports `qualOp` as an unused binding in `hedgehog-test/Main/Gen.hs`, leave it — it is still used by `PrefixQualOpAExpr` on the neighbouring line.

- [ ] **Step 11: Run the new cases**

Run:

```bash
cabal test hspec-test --enable-tests --test-options='--match "/Cases/Postgres grammar conformance/"' 2>&1 | tail -20
```

Expected: `3 examples, 0 failures`.

- [ ] **Step 12: Run the full suite and compare to baseline**

Run:

```bash
cabal test hspec-test --enable-tests --test-options="--seed 1803371098" 2>&1 | grep -E "^ +[0-9]+\) |examples,"
```

Expected: **11 failures**, and none of ExplicitRow, InExpr, OverClause, OverlayList, SubstrList, TablesampleClause, TrimList, WindowDefinition appear in the list. If any of those 8 still fails, stop and read its counterexample — the remaining cause is not the postfix production and needs diagnosing before continuing.

- [ ] **Step 13: Commit**

```bash
git add library/PostgresqlSyntax/Ast/AExpr.hs library/PostgresqlSyntax/Ast/BExpr.hs library/PostgresqlSyntax/Ast/TargetEl.hs hedgehog-test/Main/Gen.hs
git commit -m "Drop the postfix a_expr qual_Op production, removed from Postgres in v14"
```

---

### Task 3: Drop the now-redundant `safeAExprOperand` from `SortBy`

**Files:**
- Modify: `library/PostgresqlSyntax/Ast/SortBy.hs:4,52-53`

**Interfaces:**
- Consumes: `AExpr` without `SuffixQualOpAExpr` (Task 2).

This mirrors commit `f1f9f0a`, which already did the same for `FrameBound`. `SortBy` is the last remaining node-level `safeAExprOperand` user besides `TargetEl` — and `TargetEl`'s use is genuine (an implicit alias follows the expression with nothing but a space, so precedence-driven parenthesization is still needed there; leave it alone).

- [ ] **Step 1: Change the import**

In `library/PostgresqlSyntax/Ast/SortBy.hs`, replace:

```haskell
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr, filteredParser, safeAExprOperand)
```

with:

```haskell
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr, filteredParser)
```

- [ ] **Step 2: Simplify the generator**

Replace the `Qc.oneof` list body:

```haskell
      [ UsingSortBy <$> safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Qc.arbitrary <*> Qc.arbitrary,
        AscDescSortBy <$> safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Qc.arbitrary <*> Qc.arbitrary
      ]
```

with:

```haskell
      [ UsingSortBy <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary,
        AscDescSortBy <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary
      ]
```

- [ ] **Step 3: Run the properties that exercise it**

Run:

```bash
cabal test hspec-test --enable-tests --test-options='--match "/Properties/SortBy/" --match "/Properties/SortClause/" --match "/Properties/WindowDefinition/" --match "/Properties/WindowSpecification/"' 2>&1 | tail -20
```

Expected: `0 failures`.

- [ ] **Step 4: Run the full suite**

Run:

```bash
cabal test hspec-test --enable-tests --test-options="--seed 1803371098" 2>&1 | grep -E "^ +[0-9]+\) |examples,"
```

Expected: still 11 failures, same names as at the end of Task 2. If `SortBy` or any window-related property has appeared, revert this task's edit — the `safeAExprOperand` was load-bearing after all, and say so in your notes.

- [ ] **Step 5: Commit**

```bash
git add library/PostgresqlSyntax/Ast/SortBy.hs
git commit -m "Ditch redundant safeAExprOperand use in SortBy"
```

---

### Task 4: Audit the `filteredParser` exclusion lists against the reference follow sets

**Files:**
- Modify: `library/PostgresqlSyntax/Ast/FrameBound.hs:44`
- Modify: `library/PostgresqlSyntax/Ast/SortBy.hs:4,32`
- Modify: `library/PostgresqlSyntax/Ast/IndexElem.hs:48-52`
- Modify: `hspec-test/Cases.hs` (extend the `Postgres grammar conformance` group from Task 1)

**Interfaces:**
- Consumes: `rejects` and the `Postgres grammar conformance` describe group from Task 1; `AExpr.filteredParser :: [Text] -> Parser AExpr` and `AnyName.filteredParser :: [Text] -> Parser AnyName`, both unchanged.

There are exactly three `filteredParser` call sites. Two of them exclude words that are **reserved** keywords, which `Parsers.filteredColIdLike` deletes from a set they were never in (`library/PostgresqlSyntax/Helpers/Parsers.hs:212-216` deletes from `unreservedKeyword <> colNameKeyword`) — those entries are inert. Per `references/kwlist.h`: `asc` (47) and `desc` (138) and `using` (496) are `RESERVED_KEYWORD`; `nulls` (315), `preceding` (358), `following` (181), `rows` (408), `range` (375), `groups` (201), `partition` (346), `unbounded` (484) are `UNRESERVED_KEYWORD`. So the effective exclusions today are `FrameBound: [preceding, following]`, `SortBy: [nulls]`, `IndexElem: [nulls]`.

The goal here is to establish, by test, which of the three are still doing work now that the postfix production is gone, and to leave each surviving one with a comment citing the grammar rule that justifies it.

- [ ] **Step 1: Pin the behaviour each filter exists for**

Add to the `describe "Postgres grammar conformance"` group in `hspec-test/Cases.hs`:

```haskell
    -- gram.y:8596 opt_nulls_order and gram.y:14056 sortby. NULLS is
    -- unreserved (kwlist.h:315), so it is simultaneously a legal ColId and
    -- the lead-in to the nulls-order clause; Postgres separates the two
    -- readings with a two-token lexer lookahead (the NULLS_LA token,
    -- gram.y:864).
    it "sortby" $ do
      let render :: SortBy -> Text
          render = toText
      fmap render (parse @SortBy "a") `shouldBe` Right "a"
      fmap render (parse @SortBy "a asc") `shouldBe` Right "a ASC"
      fmap render (parse @SortBy "a desc nulls last") `shouldBe` Right "a DESC NULLS LAST"
      fmap render (parse @SortBy "a nulls first") `shouldBe` Right "a NULLS FIRST"
      fmap render (parse @SortBy "a using > nulls last") `shouldBe` Right "a USING > NULLS LAST"
      -- a column actually named "nulls" is a legal sort key
      fmap render (parse @SortBy "nulls") `shouldBe` Right "nulls"

    -- gram.y:8557 index_elem: ColId index_elem_options, and gram.y:8524
    -- opt_nulls_order. opt_class is an any_name, i.e. a bare ColId, so it
    -- is directly ambiguous with the unreserved NULLS that follows it.
    it "index_elem" $ do
      let render :: IndexElem -> Text
          render = toText
      fmap render (parse @IndexElem "a") `shouldBe` Right "a"
      fmap render (parse @IndexElem "a nulls first") `shouldBe` Right "a NULLS FIRST"
      fmap render (parse @IndexElem "a text_ops nulls first") `shouldBe` Right "a text_ops NULLS FIRST"
      fmap render (parse @IndexElem "a collate \"C\" text_ops desc") `shouldBe` Right "a COLLATE \"C\" text_ops DESC"
```

- [ ] **Step 2: Run them against the current (still-filtered) code**

Run:

```bash
cabal test hspec-test --enable-tests --test-options='--match "/Cases/Postgres grammar conformance/"' 2>&1 | tail -30
```

Expected: `5 examples, 0 failures`. If an assertion fails here, the *expected* string is what needs fixing (check what the renderer actually emits and correct the literal) — not the parser. Fix and re-run before continuing.

- [ ] **Step 3: Commit the pinned behaviour**

```bash
git add hspec-test/Cases.hs
git commit -m "Pin sortby and index_elem keyword-terminator behaviour"
```

- [ ] **Step 4: Try removing the `FrameBound` filter**

In `library/PostgresqlSyntax/Ast/FrameBound.hs`, replace:

```haskell
          a <- AExpr.filteredParser ["preceding", "following"]
```

with:

```haskell
          a <- AExpr.parser
```

Note `AExpr.parser` requires the `IsAst` method to be reachable through the `hs-boot`; if GHC cannot resolve it, use `(parser :: Parser AExpr)` with the existing unqualified `parser` from `PostgresqlSyntax.IsAst` instead — `AExpr.hs-boot` already declares `instance IsAst AExpr`.

Run:

```bash
cabal build all --enable-tests \
  && cabal test hspec-test --enable-tests --test-options="--seed 1803371098" 2>&1 | grep -E "^ +[0-9]+\) |examples,"
```

**Decision rule:** if the failure count and names are unchanged from Task 3 *and* the `Postgres grammar conformance` group is still green, the filter was dead — keep the removal. Otherwise `git checkout library/PostgresqlSyntax/Ast/FrameBound.hs` and go to Step 5 keeping the filter, adding this comment above it:

```haskell
          -- gram.y:17567 frame_bound. PRECEDING/FOLLOWING are unreserved
          -- (kwlist.h:358,181) and therefore legal ColIds; Postgres keeps
          -- them from being absorbed into the preceding a_expr by giving
          -- them IDENT precedence (gram.y:941), which is below every
          -- operator level. This exclusion is the recursive-descent
          -- equivalent.
          a <- AExpr.filteredParser ["preceding", "following"]
```

- [ ] **Step 5: Try removing the `SortBy` filter**

In `library/PostgresqlSyntax/Ast/SortBy.hs`, replace:

```haskell
    a <- filteredParser ["using", "asc", "desc", "nulls"]
```

with:

```haskell
    a <- parser
```

and drop `filteredParser` from the `import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (…)` list if it becomes unused.

Run the same command as Step 4. Apply the same decision rule. If the filter must stay, restore it with `"using"`, `"asc"` and `"desc"` **dropped** (they are reserved per `references/kwlist.h:496,47,138` and so are already excluded from `ColId`; listing them is inert and misleading) and this comment:

```haskell
    -- gram.y:14056 sortby. Of the four words that can terminate this
    -- a_expr, only NULLS is unreserved (kwlist.h:315) and therefore a
    -- legal ColId; USING/ASC/DESC are reserved (kwlist.h:496,47,138) and
    -- can never be absorbed. Postgres disambiguates NULLS with a
    -- two-token lexer lookahead (NULLS_LA, gram.y:864); this exclusion is
    -- the coarser recursive-descent equivalent.
    a <- filteredParser ["nulls"]
```

- [ ] **Step 6: Trim the `IndexElem` filter**

`IndexElem`'s `opt_class` is an `any_name` — a bare `ColId` — directly ambiguous with a following unreserved `NULLS`, so this one is expected to survive. Regardless of the outcome of Steps 4-5, replace the comment and list at `library/PostgresqlSyntax/Ast/IndexElem.hs:48-52`:

```haskell
      -- gram.y:8557 index_elem: ColId index_elem_options, and gram.y:8524
      -- opt_nulls_order. opt_class is an any_name, i.e. a bare ColId, so
      -- of the words that can terminate it only the unreserved NULLS
      -- (kwlist.h:315) is a genuine hazard — ASC/DESC are reserved
      -- (kwlist.h:47,138) and were never candidates.
      class_ = filteredParser ["nulls"]
```

Then run the same command as Step 4. Expected: unchanged failure set, `Postgres grammar conformance` green. If `index_elem` regresses, restore `["asc", "desc", "nulls"]` and note in your progress log which assertion broke — that would mean `filteredColIdLike` is doing something beyond `ColId` filtering and deserves its own investigation.

- [ ] **Step 7: Commit**

```bash
git add library/PostgresqlSyntax/Ast/FrameBound.hs library/PostgresqlSyntax/Ast/SortBy.hs library/PostgresqlSyntax/Ast/IndexElem.hs
git commit -m "Derive the filteredParser exclusion sets from the reference keyword classes"
```

---

### Task 5: Merge the keyword sync

**Files:**
- Modify: `library-internal/PostgresqlSyntax/KeywordSet.hs` (via merge)

The `update-keywords` branch holds commit `04d3f40` ("Update the keywords as per the latest reference"), which re-syncs `keyword`, `unreservedKeyword`, `colNameKeyword` and `reservedKeyword` against `references/kwlist.h` and drops the stale `From https://github.com/postgres/postgres/blob/1aac32df…` comment. It is a descendant of `84d0c25` but is not on this branch. It adds ~50 words (`absent`, `json*`, `merge*`, `format`, `keys`, `nested`, `path`, `scalar`, `period`, …) and removes `recheck`.

Task 4 derives its exclusion sets from these classifications, so merging after Task 4 keeps the two changes separately bisectable.

- [ ] **Step 1: Merge**

```bash
git merge --no-ff update-keywords -m "Merge the keyword sync against the pinned kwlist.h"
```

- [ ] **Step 2: Build and test**

```bash
cabal build all --enable-tests \
  && cabal test hspec-test --enable-tests --test-options="--seed 1803371098" 2>&1 | grep -E "^ +[0-9]+\) |examples,"
```

- [ ] **Step 3: Triage**

**Decision rule.** Compare the failure set to the end of Task 4.

- Unchanged → go to Step 4.
- New failures whose counterexamples turn on a word's *classification* (a word that is now unreserved being accepted or rejected as a `ColId`, an exclusion list that now needs a word it didn't before) → fix here, one commit per fix, citing the `references/kwlist.h` line.
- New failures that need a **grammar production this library doesn't have** (`MERGE`, `JSON_TABLE`, `json_*` functions, `graph_table`, `PERIOD`, …) → **out of scope**. Revert the merge (`git merge --abort`, or `git revert -m 1 <merge-sha>` if already committed), and open a follow-up note in `notes.md` recording exactly which productions are missing. Adding new statement/expression forms is a different plan.

- [ ] **Step 4: Commit (only if the merge was not reverted)**

The merge commit is already made by Step 1; commit any triage fixes separately with messages naming the keyword and its `kwlist.h` line.

---

### Task 6: Changelog

**Files:**
- Modify: `CHANGELOG.md` (the existing `# Upcoming` → `## Breaking` list)

- [ ] **Step 1: Add the entry**

Append to the `## Breaking` list under `# Upcoming`:

```markdown
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
```

- [ ] **Step 2: Verify the file still renders as valid Markdown**

Run:

```bash
head -40 CHANGELOG.md
```

Expected: the new bullet sits inside `## Breaking`, above `## Non-breaking` if one exists.

- [ ] **Step 3: Full verification**

```bash
cabal build all --enable-tests
cabal test hspec-test --enable-tests --test-options="--seed 1803371098" 2>&1 | tail -5
```

Expected: 11 failures, all from the out-of-scope clusters listed in the Baseline table. Paste the output.

- [ ] **Step 4: Commit**

```bash
git add CHANGELOG.md
git commit -m "Changelog: postfix operator removal"
```

---

## Explicitly out of scope

These are real, currently-failing round-trip properties with different root causes. Do not fix them in this plan; each deserves its own diagnosis.

1. **`Op "?"` vs the `Typename` nullability extension** (ForLockingItem, SelectClause). This library accepts `int4?` as a nullable typename — an extension with no counterpart in `references/gram.y`. `?` is a legal Postgres operator character, so `x :: int4 ? y` is ambiguous between the extension and a `SymbolicBinOpAExpr`. Removing the postfix production removes most instances but not the binary-operator ones.
2. **`SubqueryAExpr`** (CallStmt, DeleteStmt, FuncApplication, SimpleSelect, UpdateStmt, WhenClause, WithClause) — `a_expr subquery_Op sub_type select_with_parens` (gram.y:16294,16306). Seven properties.
3. **`AllIndirectionEl` (`.*`)** reaching table-ref positions that reject it (JoinedTable). Likely a generator-side over-generation rather than a parser bug.
4. **PreparableStmt** — unclassified; re-shrink it once 1-3 are done, it may be a duplicate of one of them.
5. **Restoring `hedgehog-test`** and the other items already queued in `notes.md`.

## Self-review notes

- **Spec coverage.** The originating question was "the AExpr parser consumes keywords — is that the root?" Answer, established against `references/gram.y`: no, `ColId: IDENT | unreserved_keyword | col_name_keyword` (gram.y:18853) means Postgres's identifier parser accepts keywords too, so that behaviour is required, not a bug. The genuine divergence is the postfix `qual_Op` production (Task 2), and the residual keyword-terminator handling is a follow-set problem that Postgres solves with a precedence table (gram.y:940-942) plus lexer lookahead (gram.y:864) — Task 4 re-derives the local equivalents from those two sources.
- **Not addressed by design:** propagating a terminator set through *all* of `customizedParser`'s recursive calls. Several sub-expressions call the unfiltered `parser` rather than the filtered `aExpr` (the `BETWEEN` operand at `AExpr.hs:282`, the `SubqueryAExpr` components, `CollateAExpr`'s tail). That is a latent hole today, but with the postfix production gone there is no known input that exercises it, and building a parser-context mechanism for a hazard with no failing test would be speculative. If Task 4 Step 4 or 5 shows the filters are *not* dead, revisit this.
- **Type consistency:** `rejects` is defined in Task 1 Step 2 and used in Task 1 Step 3 only. `AExpr.filteredParser :: [Text] -> Parser AExpr` and `AnyName.filteredParser :: [Text] -> Parser AnyName` keep their signatures throughout; no `hs-boot` change is needed at any point.
