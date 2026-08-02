{-# LANGUAGE AllowAmbiguousTypes #-}

module PostgresqlSyntax.Algebra
  ( IsAst (..),
    toText,
    parse,
    parseWithPosError,
    parseWithSourcePosError,
    isAstProperties,
    Canonicalizes (..),
    canonicalizesProperties,
    Refines (..),
    refinesProperties,
    LeftRecursive (..),
    Extends (..),
    extendedByProperties,
    parseMaybeExtended,
    parseExtended,
    parseExtensionChain,
    Parser,
  )
where

import qualified Data.Text as Text
import qualified HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Extras
import PostgresqlSyntax.Prelude
import PostgresqlSyntax.Settings (Settings)
import qualified Test.QuickCheck as Qc
import qualified Text.Megaparsec as Megaparsec
import qualified TextBuilder

-- |
-- Class of AST types that can be rendered to SQL text and parsed back again.
--
-- Laws:
--
-- * __Roundtrips__: @parse settings (toText settings a) = Right a@ for every
--   'Settings' — rendering and parsing are inverses.
-- * __Congruent rendering__: @a == b => toTextBuilder settings a == toTextBuilder settings b@
--   for every 'Settings' — rendering only depends on the value, not on how it
--   was constructed. This is what makes it meaningful to say two structurally
--   different shapes can still render to identical text — the ambiguity that
--   'Canonicalizes' exists to resolve.
class IsAst a where
  -- |
  -- Render an AST value to a 'TextBuilder' using the given 'Settings'.
  -- This is the low-level rendering primitive; 'toText' wraps it.
  toTextBuilder :: Settings -> a -> TextBuilder

  -- |
  -- A parser for this AST type, parameterized by 'Settings'.
  -- The parser must satisfy the roundtrip law documented on 'IsAst'.
  parser :: Settings -> Parser a

-- |
-- 'Qc.Property'-checkers for 'IsAst'\'s documented laws, keyed by name.
isAstProperties :: forall a. (IsAst a, Eq a, Show a, Qc.Arbitrary a) => [(String, Qc.Property)]
isAstProperties =
  [ ( "Roundtrips",
      Qc.property $ \(a :: a) ->
        let sql = toText mempty a
         in case parse mempty sql of
              Left err ->
                Qc.counterexample ("rendered: " <> Text.unpack sql <> "\nparse failed: " <> Text.unpack err) False
              Right a' ->
                Qc.counterexample
                  ("rendered: " <> Text.unpack sql <> "\nrestored: " <> Text.unpack (toText mempty a'))
                  (a' Qc.=== a)
    ),
    ( "Renders equal values equally",
      Qc.property $ \(a :: a) (b :: a) ->
        a /= b || toText mempty a == toText mempty b
    )
  ]

-- |
-- Render a value to 'Text' via its 'toTextBuilder' method.
toText :: (IsAst a) => Settings -> a -> Text
toText settings = TextBuilder.toText . toTextBuilder settings

-- |
-- Parse a 'Text' input with the type's 'parser', returning either a
-- pretty-printed error or the parsed value. The parser is chosen by the
-- caller's type inference (via the 'IsAst' constraint), so callers no longer
-- pass an explicit parser argument.
parse :: (IsAst a) => Settings -> Text -> Either Text a
parse settings = first Text.pack . Extras.run (Extras.totally (parser settings))

-- |
-- Like 'parse' but returns the structured error list (each error paired with
-- its byte offset) instead of a single pretty-printed message.
parseWithPosError :: (IsAst a) => Settings -> Text -> Either (NonEmpty (Int, Text)) a
parseWithPosError settings = first (fmap (second Text.pack)) . Extras.runParserWithErrorPos (Extras.totally (parser settings))

-- |
-- Like 'parseWithPosError' but pairs each error with its
-- 'Text.Megaparsec.SourcePos' instead of a raw byte offset.
parseWithSourcePosError :: (IsAst a) => Settings -> Text -> Either (NonEmpty (Megaparsec.SourcePos, Text)) a
parseWithSourcePosError settings = first (fmap (second Text.pack)) . Extras.runParserWithSourcePosError (Extras.totally (parser settings))

-- |
-- Laws:
--
-- * __Idempotent__: @canonicalize . canonicalize = canonicalize@
-- * __Parse-agreement__ (the property this class exists to provide):
--   @parse settings . toText settings = Right . canonicalize@ for every
--   'PostgresqlSyntax.Settings.Settings'
class (IsAst a) => Canonicalizes a where
  canonicalize :: a -> a
  canonicalize = id

-- |
-- 'Qc.Property'-checkers for 'Canonicalizes'\'s documented laws, keyed by
-- name. \"Parse-agreement\" is tested at 'mempty' 'Settings', matching how
-- 'isAstProperties'\'s \"Renders equal values equally\" property handles \"for
-- every 'Settings'\" — no 'Qc.Arbitrary' 'Settings' instance exists or is
-- being added.
canonicalizesProperties :: forall a. (Canonicalizes a, Eq a, Show a, Qc.Arbitrary a) => [(String, Qc.Property)]
canonicalizesProperties =
  [ ( "Idempotent",
      Qc.property $ \(a :: a) ->
        canonicalize (canonicalize a) Qc.=== canonicalize a
    ),
    ( "Parse-agreement",
      Qc.property $ \(a :: a) ->
        parse mempty (toText mempty a) Qc.=== Right (canonicalize a)
    )
  ]

-- |
-- Laws:
--
-- * __Refinement law__: @project . embed = Just@
--
-- Expresses embedding relationships between AST node types across module
-- boundaries where cross-module pattern matching is unavailable. Instances
-- hold between two types when the 'sub' type can be trivially embedded into
-- 'sup', and trivial 'sup' values can be recognized as such a 'sub' and
-- extracted back out.
class Refines sub sup where
  embed :: sub -> sup
  project :: sup -> Maybe sub

-- |
-- 'Qc.Property'-checkers for 'Refines'\'s documented laws, keyed by name.
refinesProperties :: forall sub sup. (Refines sub sup, IsAst sub, Eq sub, Show sub, Qc.Arbitrary sub) => [(String, Qc.Property)]
refinesProperties =
  [ ( "Refinement law",
      Qc.property $ \(a :: sub) ->
        project (embed a :: sup) Qc.=== Just a
    )
  ]

-- |
-- A type some of whose grammar productions are left-recursive — i.e. there
-- is a larger recursive form built by extending a value of this type on its
-- left. 'parseBase' is everything that is /not/ one of those productions:
-- the @β@ of @A -> Aα | β@.
--
-- This is a strictly weaker claim than 'Extends', which additionally
-- names the specific @ext@ of one such hub. A type can be 'LeftRecursive'
-- without being any hub's @base@ — 'PostgresqlSyntax.Ast.JoinedTable' and
-- 'PostgresqlSyntax.Ast.SimpleSelect' both are, since each is reached by
-- extending a /different/ type (@table_ref@ and @select_clause@
-- respectively) yet still has non-left-recursive productions of its own.
--
-- Separating this from 'Extends' is what lets each instance live with
-- the type it constructs: 'parseBase' mentions only its own type, so it
-- belongs to that type's module, while a hub's 'parseExtensions' belongs to
-- the module defining @ext@. Because both are class methods, either module
-- can reach the other's parser through an @hs-boot@ instance declaration
-- without exporting a bare helper.
--
-- Note that \"non-recursive\" here means non-/left/-recursive only: a
-- production may still recurse, so long as it doesn't begin with the
-- recursive position (e.g. @'(' joined_table ')'@).
class (IsAst a) => LeftRecursive a where
  -- | Parse only the productions that don't left-recurse (@β@).
  parseBase :: Settings -> Parser a

-- |
-- The two halves of a left-recursive grammar production, split apart by
-- left-recursion elimination (@A -> Aα | β@ becomes @A -> β α*@): 'base' is
-- @A@ (and supplies @β@ via its 'LeftRecursive' instance), and 'ext' is
-- what one or more @α@\'s, applied to a @base@, produce.
--
-- Laws:
--
-- * __Base-parser agreement__: @parser \@base = parseMaybeExtended \@base@
-- * __Maximal munch__: 'parseExtensions' must not return while a further
--   extension is available — instances build this on 'parseExtensionChain'
--   where possible, which already guarantees it.
class (LeftRecursive base, Refines ext base) => Extends base ext | ext -> base where
  -- | Parse one or more extensions onto an already-parsed left operand,
  -- folding as it goes, and return the fully-extended result.
  parseExtensions :: Settings -> base -> Parser ext

-- |
-- 'Qc.Property'-checker for 'Extends'\'s \"Base-parser agreement\" law,
-- keyed by name. \"Maximal munch\" isn't checked here: it's a per-instance
-- parsing obligation, not something a generated 'base' value can exercise
-- through 'parser' alone. The agreement check is itself up to whether
-- parsing succeeds, ignoring error message text, since a base's 'parser'
-- may wrap 'parseMaybeExtended' in a 'HeadedMegaparsec.label' or similar
-- that changes failure messages without changing what's accepted.
extendedByProperties :: forall base ext. (Extends base ext, IsAst base, Eq base, Show base, Qc.Arbitrary base) => [(String, Qc.Property)]
extendedByProperties =
  [ ( "Base-parser agreement",
      Qc.property $ \(a :: base) ->
        let sql = toText mempty a
            run p = first (const ()) (Extras.run (Extras.totally p) sql)
         in run (parser @base mempty) Qc.=== run (parseMaybeExtended @ext mempty)
    )
  ]

-- |
-- Parses zero or more extensions onto a 'parseBase', via 'parseExtensions'.
-- This is what @A -> β α*@ (the whole of @A@) means as a parser.
parseMaybeExtended :: forall ext base. (Extends base ext) => Settings -> Parser base
parseMaybeExtended settings = do
  b <- parseBase @base settings
  optional (parseExtensions @base settings b) >>= maybe (pure b) (pure . embed @ext @base)

-- |
-- Like 'parseMaybeExtended', but requires at least one extension to follow
-- the base, and so returns the fully-applied 'ext' type directly rather
-- than 'base'. This is what a bare @α*@ (one or more) means as a parser,
-- for hubs where a chain of at least one extension is itself the
-- interesting type (e.g. a @joined_table@, which is never a bare
-- @table_ref@ with zero joins).
--
-- Unlike 'parseMaybeExtended', 'parseBase' here is wrapped in
-- 'Parser.wrapToHead'. Without it, if 'parseBase' itself commits past an
-- internal 'Parser.endHead' (e.g. by matching a nested, fully-parenthesized
-- instance of the very thing this function's caller is one alternative
-- for), that commitment silently swallows the immediately-following "is
-- there at least one extension?" check: a missing extension would fail as
-- a hard, uncatchable error instead of a clean one this function's own
-- caller can backtrack from. 'wrapToHead' resets that, forcing the check
-- to fail cleanly. 'parseMaybeExtended' doesn't need this: its own
-- extension check already goes through 'optional', which independently
-- wraps in 'Megaparsec.try' regardless of what 'parseBase' committed to.
parseExtended :: forall base ext. (Extends base ext) => Settings -> Parser ext
parseExtended settings = do
  b <- Parser.wrapToHead (parseBase @base settings)
  parseExtensions @base settings b

-- |
-- Parses one or more items back-to-back, wrapping each in
-- 'Parser.wrapToHead'\/'Parser.endHead' so that, once an item's own head has
-- matched, backtracking out of the whole chain (back to "there are no more
-- items") is no longer attempted — matching the hand-written
-- recursive-descent loops this replaces. This is the shared backtracking
-- protocol underlying 'Extends'\'s \"Maximal munch\" law: an instance
-- building 'parseExtensions' on top of this combinator gets the law for
-- free, since 'go' only stops once a further item genuinely isn't
-- available.
parseExtensionChain :: Parser item -> Parser (NonEmpty item)
parseExtensionChain item = go
  where
    go = do
      i <- Parser.wrapToHead item
      Parser.endHead
      rest <- optional go
      pure $ case rest of
        Nothing -> i :| []
        Just (j :| js) -> i :| j : js

type Parser = Parser.HeadedParsec Void Text
