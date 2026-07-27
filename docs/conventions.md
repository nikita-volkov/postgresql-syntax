# Project-specific coding conventions

This project follows [nikita-volkov/haskell-coding-standards](https://github.com/nikita-volkov/haskell-coding-standards)
(pinned in `.haskell-coding-standards.lock`). The deviations below are
intentional, project-local overrides of that standard — apply them here even
where they contradict the upstream doc.

## Imports

### `Test.QuickCheck` is qualified, not treated as a topic import

Upstream's `conventions/imports.md` lists `Test.QuickCheck` in a module that
defines an `Arbitrary` instance as a *topic* import (left unqualified). In
this project's `PostgresqlSyntax.Ast.*` modules, each module already has a
primary topic — the AST node type and its `IsAst` instance (parser +
renderer) — and the `Arbitrary` instance is a secondary concern bolted on for
testing. QuickCheck is therefore qualified as `Qc`:

```haskell
import qualified Test.QuickCheck as Qc

instance Qc.Arbitrary Bconst where
  arbitrary = do
    len <- Qc.choose (1, 100)
    Bconst . Text.pack <$> Qc.vectorOf len (Qc.elements "01")
```

### `HeadedMegaparsec` (+ its `Extras` module) is aliased as `Parser`, not left bare

Upstream's "external packages — well-named" rule says a single-component
package name like `HeadedMegaparsec` gets no alias, full path at call sites.
Here it's aliased to `Parser` instead, because `PostgresqlSyntax.Prelude`
already defines:

```haskell
type Parser = HeadedMegaparsec.HeadedParsec Void Text
```

Call sites read as operations building/consuming that `Parser` type rather
than as calls into the `HeadedMegaparsec` package:

```haskell
import qualified HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser

parser = Parser.label "bit literal" $ do
  Parser.string' "b'"
  Parser.endHead
  ...
```

`PostgresqlSyntax.Extras.HeadedMegaparsec` (this project's own extension
module for `HeadedMegaparsec`) is folded into the *same* `Parser` alias
rather than getting its own — the two modules jointly form one namespace of
parser combinators, and call sites shouldn't need to know which of the two a
given combinator came from.
