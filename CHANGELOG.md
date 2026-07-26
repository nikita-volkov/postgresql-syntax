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
