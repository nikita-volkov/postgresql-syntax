# Task: eliminate exponential parsing time on nested expressions

## Status

Open. To be executed in a dedicated session, starting from `master`.

## Problem

`PostgresqlSyntax.Parsing` takes time exponential in the nesting depth of an
expression. Parsing cost roughly multiplies with every additional level of
parentheses, so inputs that are trivial by any human measure become unparseable
in practice.

This is not a micro-optimisation issue. Realistic queries — hand-written SQL with
a few layers of grouping around a long arithmetic expression — hang the parser
indefinitely. The package is unusable for such input.

## Reproduction

Two cases, both parsed with the expression entry point (`aExpr`) or via the
top-level statement parser:

1. **Synthetic.** A minimal binary expression wrapped in N layers of redundant
   parentheses: `((((a + b))))`, scaled up. Time should be measured as a function
   of N. On the current code the growth is visibly exponential; somewhere in the
   low double digits of N the parse stops finishing at all.

2. **Realistic.** A sum-of-`COALESCE` expression of a few dozen terms, split into
   two parenthesised groups subtracted from one another, with the whole thing
   wrapped in ~6 redundant parentheses. This is the shape reported by users. It
   terminates on the current code, but takes tens of seconds.

Both cases must be captured before any change is made, with recorded numbers, so
the effect of the fix is demonstrable rather than asserted.

## Goal

Parsing time must be linear (or near-linear) in input size, and independent of
nesting depth beyond the linear contribution of the extra characters. Concretely:

- Case 1 at a depth that currently does not terminate must parse in well under a
  second.
- Case 2 must parse in milliseconds.
- No regression in parsing of anything else.

## Constraints

- **No change to the public AST or the rendering behaviour** unless it is proven
  necessary. The package's contract is `parse . render == id` on well-formed
  trees; that must continue to hold.
- **The full existing test suites must pass** — both the example-based tests and
  the property tests that round-trip generated syntax trees.
- Error messages must not degrade into uselessness. The parser deliberately uses
  a mechanism for committing to a branch once it is unambiguously identified;
  whatever the fix, the resulting failure messages for genuinely invalid input
  should still point at the right place. Check a handful by hand.

## Known complication: ambiguous parses

At least one construct in the grammar has two distinct AST representations of the
same source text — a parenthesised sub-select can be represented either as a
generic parenthesised expression wrapping a select, or as a select carrying its
own parentheses. The property-test generators currently emit only one of the two
shapes, which means the ambiguity is masked rather than resolved.

Whoever executes this task will hit this. It is a **specification decision**, not
an implementation detail:

- Decide which of the two shapes is canonical for the parser to produce.
- Make that decision explicit — in the code and, if it is user-visible, in the
  documentation.
- Align the generators and tests with the decision rather than working around it.

Do not let the incidental shape that falls out of whatever restructuring you
choose silently become the answer.

## Suggested approach to the work

1. Reproduce and measure first. Add a benchmark (or at minimum a repeatable
   timing script) covering both cases above; commit it before the fix.
2. Diagnose from the measurements — identify which parts of the grammar actually
   blow up, rather than fixing everything that looks suspicious. The expression
   grammar is the hot path; changes elsewhere are likely wasted effort and add
   review risk.
3. Fix, keeping the diff as small and as local as the problem allows. The
   parser's existing structure is already hard to follow; a fix that makes it
   substantially harder to follow trades one problem for another, and needs an
   explicit justification if chosen.
4. Verify with the recorded before/after numbers and the full test suite.
5. Record the outcome in `CHANGELOG.md`.

## Non-goals

- Rewriting the parser or changing its underlying parsing library.
- Restructuring the grammar for elegance where performance is not affected.
- Fixing unrelated ordering sensitivities between grammar alternatives, if any
  are found. Note them; don't chase them.

## Reference

Origin: <https://github.com/nikita-volkov/postgresql-syntax/pull/8>. Read it only
after forming your own diagnosis, if at all — it contains one particular
solution, and this task is deliberately written to leave the solution open.
