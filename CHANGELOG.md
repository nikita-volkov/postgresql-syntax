# Upcoming

## Breaking

- A top-level parenthesized select (e.g. `(select 1)`) now parses as `Right` of `selectStmt` (a `SelectWithParens`) instead of `Left` of a `SelectNoParens` wrapping it (#8)

## Fixes

- Fix exponential parsing time on nested parentheses (#8)

- Fix keyword error reporting under megaparsec >=9.8 (#20)
- Fix OFFSET/aExpr round-trip failure for `OPERATOR(...)` prefix (#11, #22)
- Fix hedgehog generator for `type_function_name` to use its own keyword set
