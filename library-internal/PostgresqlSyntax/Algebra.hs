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
    LeftRecursion (..),
    leftRecursionProperties,
    parseLeftRecursive,
    parseExtended,
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
  toTextBuilder :: Settings -> a -> TextBuilder
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
-- left. 'nonRecursiveParser' is everything that is /not/ one of those
-- productions: the @β@ of @A -> Aα | β@.
--
-- This is a strictly weaker claim than 'LeftRecursion', which additionally
-- names the specific @ext@ and @item@ of one such hub. A type can be
-- 'LeftRecursive' without being any hub's @base@ —
-- 'PostgresqlSyntax.Ast.JoinedTable' and
-- 'PostgresqlSyntax.Ast.SimpleSelect' both are, since each is reached by
-- extending a /different/ type (@table_ref@ and @select_clause@
-- respectively) yet still has non-left-recursive productions of its own.
--
-- Separating this from 'LeftRecursion' is what lets each instance live with
-- the type it constructs: 'nonRecursiveParser' mentions only its own type,
-- so it belongs to that type's module, while a hub's 'extension' and
-- 'applyExtension' belong to the module defining @ext@. Because both are
-- class methods, either module can reach the other's parser through an
-- @hs-boot@ instance declaration without exporting a bare helper.
--
-- Note that \"non-recursive\" here means non-/left/-recursive only: a
-- production may still recurse, so long as it doesn't begin with the
-- recursive position (e.g. @'(' joined_table ')'@).
class (IsAst a) => LeftRecursive a where
  -- | Parse only the productions that don't left-recurse (@β@).
  nonRecursiveParser :: Settings -> Parser a

-- |
-- The two halves of a left-recursive grammar production, split apart by
-- left-recursion elimination (@A -> Aα | β@ becomes @A -> β α*@): 'base' is
-- @A@ (and supplies @β@ via its 'LeftRecursive' instance), 'ext' is what a
-- fully-applied @α@ produces, and 'item' is a single @α@ with its left
-- operand removed (since that operand is supplied externally, by whatever's
-- folding the chain).
--
-- Laws:
--
-- * __Base-parser agreement__: @parser \@base = parseLeftRecursive \@base@
-- * __Left fold__: 'foldExtensions'\'s default applies its first item
--   to 'base', then folds every remaining item onto that result via
--   'embed' — the first item is applied innermost.
class (LeftRecursive base, Refines ext base) => LeftRecursion base ext item | base -> ext item where
  -- | Parse a single @α@, minus the left operand it would apply to.
  extension :: Settings -> Parser item

  -- | Apply one parsed 'item' to a left operand.
  applyExtension :: base -> item -> ext

  -- | Fold a chain of one or more parsed items onto a base. Overridden
  -- where a hub's items aren't all the same precedence (see
  -- "PostgresqlSyntax.Ast.SimpleSelect").
  foldExtensions :: base -> NonEmpty item -> ext
  foldExtensions b (i :| is) = foldl' (\acc j -> applyExtension @base @ext @item (embed acc) j) (applyExtension b i) is

-- |
-- 'Qc.Property'-checker for 'LeftRecursion'\'s \"Base-parser agreement\"
-- law, keyed by name. \"Left fold\" isn't checked here: it documents what
-- 'foldExtensions'\'s /default/ does, not a law every instance must satisfy
-- — 'PostgresqlSyntax.Ast.SimpleSelect'\'s instance deliberately overrides
-- it to fold items of differing precedence instead. The agreement check is
-- itself up to whether parsing succeeds, ignoring error message text, since
-- a base's 'parser' may wrap 'parseLeftRecursive' in a
-- 'HeadedMegaparsec.label' or similar that changes failure messages without
-- changing what's accepted.
leftRecursionProperties :: forall base ext item. (LeftRecursion base ext item, IsAst base, Eq base, Show base, Qc.Arbitrary base) => [(String, Qc.Property)]
leftRecursionProperties =
  [ ( "Base-parser agreement",
      Qc.property $ \(a :: base) ->
        let sql = toText mempty a
            run p = first (const ()) (Extras.run (Extras.totally p) sql)
         in run (parser @base mempty) Qc.=== run (parseLeftRecursive @base @ext @item mempty)
    )
  ]

-- |
-- Parses zero or more 'extension's onto a 'nonRecursiveBase', folding them
-- via 'foldExtensions'. This is what @A -> β α*@ (the whole of @A@) means
-- as a parser.
parseLeftRecursive :: forall base ext item. (LeftRecursion base ext item) => Settings -> Parser base
parseLeftRecursive settings = do
  b <- nonRecursiveParser @base settings
  optional (parseItems @base @ext @item settings) >>= \case
    Nothing -> pure b
    Just items -> pure (embed (foldExtensions b items))

-- |
-- Like 'parseLeftRecursive', but requires at least one 'extension' to
-- follow the base, and so returns the fully-applied 'ext' type directly
-- rather than 'base'. This is what a bare @α*@ (one or more) means as a
-- parser, for hubs where a chain of at least one extension is itself the
-- interesting type (e.g. a @joined_table@, which is never a bare
-- @table_ref@ with zero joins).
--
-- Unlike 'parseLeftRecursive', 'nonRecursiveParser' here is wrapped in
-- 'Parser.wrapToHead'. Without it, if 'nonRecursiveParser' itself commits
-- past an internal 'Parser.endHead' (e.g. by matching a nested,
-- fully-parenthesized instance of the very thing this function's caller
-- is one alternative for), that commitment silently swallows the
-- immediately-following "is there at least one extension?" check: a
-- missing extension would fail as a hard, uncatchable error instead of a
-- clean one this function's own caller can backtrack from. 'wrapToHead'
-- resets that, forcing the check to fail cleanly. 'parseLeftRecursive'
-- doesn't need this: its own extension check already goes through
-- 'optional', which independently wraps in 'Megaparsec.try' regardless of
-- what 'nonRecursiveParser' committed to.
parseExtended :: forall base ext item. (LeftRecursion base ext item) => Settings -> Parser ext
parseExtended settings = do
  b <- Parser.wrapToHead (nonRecursiveParser @base settings)
  items <- parseItems @base @ext @item settings
  pure (foldExtensions b items)

-- |
-- Parses one or more 'extension's back-to-back, wrapping each in
-- 'Parser.wrapToHead'\/'Parser.endHead' so that, once an extension's own
-- head has matched, backtracking out of the whole chain (back to "there
-- are no more extensions") is no longer attempted — matching the
-- hand-written recursive-descent loops this replaces.
parseItems :: forall base ext item. (LeftRecursion base ext item) => Settings -> Parser (NonEmpty item)
parseItems settings = go
  where
    go = do
      i <- Parser.wrapToHead (extension @base @ext @item settings)
      Parser.endHead
      rest <- optional go
      pure $ case rest of
        Nothing -> i :| []
        Just (j :| js) -> i :| j : js
