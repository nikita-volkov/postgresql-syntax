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
  )
where

import qualified Data.Text as Text
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
