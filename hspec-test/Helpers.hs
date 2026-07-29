{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Shared plumbing for the per-node spec modules under "Ast".
--
-- Kept out of any single node's Spec module and out of the "PostgresqlSyntax"
-- facade so that a node's Spec file only needs to import its own
-- "PostgresqlSyntax.Ast.*" module plus this one, without recompiling on
-- changes to unrelated AST nodes.
module Helpers
  ( -- * Property groups
    fullSpec,
    onlyArbitrarySpec,

    -- * Example-based parse helpers
    parsesTo,
    reportsError,
    parsesWithin,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import PostgresqlSyntax.IsAst
import Prelude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, (===))
import qualified Test.QuickCheck as Qc

-- | The property group every AST node type that round-trips through
-- 'parse' \/ 'toText' must satisfy: a parse/render round-trip ('IsAst') and
-- a bounded 'Arbitrary' generator.
fullSpec :: forall a. (IsAst a, Eq a, Show a, Qc.Arbitrary a) => Spec
fullSpec = do
  describe "IsAst"
    $ prop "Roundtrips"
    $ \(a :: a) ->
      let sql = toText a
       in case parse sql of
            Left err ->
              counterexample ("rendered: " <> toList sql <> "\nparse failed: " <> err) False
            Right a' ->
              counterexample
                ("rendered: " <> toList sql <> "\nrestored: " <> toList (toText a'))
                (a' === a)
  arbitrarySpec @a

-- | Like 'fullSpec' minus the round-trip property, for node types whose
-- renderings only round-trip when embedded after other text (so they can't
-- be parsed standalone as a top-level target).
onlyArbitrarySpec :: forall a. (IsAst a, Show a, Qc.Arbitrary a) => Spec
onlyArbitrarySpec = arbitrarySpec @a

-- | The 'Arbitrary' generator-bounds sub-group, shared by 'fullSpec' and
-- 'onlyArbitrarySpec'.
arbitrarySpec :: forall a. (IsAst a, Show a, Qc.Arbitrary a) => Spec
arbitrarySpec =
  -- Two invariants every 'Arbitrary' instance in this library must satisfy,
  -- independent of parsing (hence the only properties run for the node types
  -- that can't round-trip as a top-level parse target — see
  -- 'onlyArbitrarySpec'):
  --
  -- 1. 'terminatesAtZero': at size 0 the generator must escape every recursive
  --    strongly-connected component and yield a small value. A non-escaping
  --    base case turns size-0 generation into an unbounded random walk that
  --    renders to arbitrarily deep nesting.
  -- 2. 'growsBounded': at the suite's maximum size (hspec's default 'maxSize'
  --    is 100) the rendered output must stay within a budget, so a generator
  --    that explodes super-linearly — e.g. a list whose length doesn't consume
  --    the size budget — is caught by its output length rather than by a stack
  --    overflow deep inside a round-trip prop.
  describe "Arbitrary" $ do
    prop "Terminates at size 0" terminatesAtZero
    prop "Grows boundedly" growsBounded
  where
    terminatesAtZero =
      Qc.forAll (Qc.resize 0 (Qc.arbitrary @a)) $ \x ->
        let sql = toText x
            len = Text.length sql
         in Qc.counterexample
              ("rendered " <> show len <> " chars at size 0 (max " <> show zeroSizeMaxLen <> ")" <> "\n" <> toList sql)
              (len <= zeroSizeMaxLen)
      where
        -- Rendered-length ceiling for size-0 generation. A well-behaved
        -- generator produces a leaf at size 0, so this only ever trips on a
        -- non-terminating base case (which renders unbounded nesting).
        zeroSizeMaxLen = 500

    growsBounded =
      Qc.forAll (Qc.resize maxGenSize (Qc.arbitrary @a)) $ \x ->
        let sql = toText x
            len = Text.length sql
         in Qc.counterexample
              ("rendered " <> show len <> " chars at size " <> show maxGenSize <> " (max " <> show maxGenSizeMaxLen <> ")")
              (len <= maxGenSizeMaxLen)
      where
        -- The size at which the growth bound is measured. Matches hspec's
        -- default 'maxSize', i.e. the largest size any prop in this suite is
        -- run at.
        maxGenSize = 100
        -- Rendered-length budget at 'maxGenSize'. Catches super-linear (e.g.
        -- quasi-polynomial) explosion that a @div 2@-per-edge rule doesn't
        -- bound.
        maxGenSizeMaxLen = 1000000

-- * Example-based parse helpers

parsesTo :: forall a. (HasCallStack, IsAst a) => Text -> Expectation
parsesTo input =
  case parse @a input of
    Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
    Right _ -> pure ()

reportsError :: forall a. (HasCallStack, IsAst a) => Text -> String -> Expectation
reportsError input expected =
  case parseWithPosError @a input of
    Left err -> show (NonEmpty.head err) `shouldBe` expected
    Right _ -> expectationFailure "expected a parse error, but it succeeded"

parsesWithin :: forall a. (HasCallStack, IsAst a) => Int -> Text -> Expectation
parsesWithin seconds input = do
  result <-
    timeout (seconds * 1000000)
      $ case parse @a input of
        Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
        Right _ -> pure ()
  case result of
    Nothing -> expectationFailure ("Did not finish parsing within " <> show seconds <> "s")
    Just () -> pure ()
