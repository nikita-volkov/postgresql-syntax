{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Shared 'Spec' definitions for the per-node spec modules under "Ast".
--
-- Kept out of any single node's Spec module and out of the "PostgresqlSyntax"
-- facade so that a node's Spec file only needs to import its own
-- "PostgresqlSyntax.Ast.*" module plus this one, without recompiling on
-- changes to unrelated AST nodes.
module Helpers.Specs
  ( -- * Property groups
    itSatisfiesIsAst,
    itSatisfiesArbitrary,

    -- * Spec-level parse helpers
    itParses,
    itRejects,
    itReportsError,
    itParsesWithin,
  )
where

import qualified Data.Text as Text
import qualified Helpers.Expectations as Expectations
import PostgresqlSyntax.IsAst
import Prelude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, (===))
import qualified Test.QuickCheck as Qc

-- | The round-trip property: every AST node type that goes through
-- 'parse' \/ 'toText' must satisfy 'IsAst'.
itSatisfiesIsAst :: forall a. (IsAst a, Eq a, Show a, Qc.Arbitrary a) => Spec
itSatisfiesIsAst =
  describe "IsAst" $ do
    prop "Roundtrips" $ \(a :: a) ->
      let sql = toText a
       in case parse sql of
            Left err ->
              counterexample ("rendered: " <> toList sql <> "\nparse failed: " <> err) False
            Right a' ->
              counterexample
                ("rendered: " <> toList sql <> "\nrestored: " <> toList (toText a'))
                (a' === a)

-- | The 'Arbitrary' generator-bounds sub-group.
itSatisfiesArbitrary :: forall a. (IsAst a, Show a, Qc.Arbitrary a) => Spec
itSatisfiesArbitrary =
  -- Two invariants every 'Arbitrary' instance in this library must satisfy,
  -- independent of parsing (hence the only properties run for the node types
  -- that can't round-trip as a top-level parse target):
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
        zeroSizeMaxLen = 500

    growsBounded =
      Qc.forAll (Qc.resize maxGenSize (Qc.arbitrary @a)) $ \x ->
        let sql = toText x
            len = Text.length sql
         in Qc.counterexample
              ("rendered " <> show len <> " chars at size " <> show maxGenSize <> " (max " <> show maxGenSizeMaxLen <> ")")
              (len <= maxGenSizeMaxLen)
      where
        maxGenSize = 100
        maxGenSizeMaxLen = 1000000

-- * Spec-level parse helpers

-- | Wraps 'Expectations.parsesTo' as a standalone 'Spec'.
itParses :: forall a. (HasCallStack, IsAst a) => Text -> Spec
itParses sql =
  describe (Text.unpack sql) $ do
    it "Parses" (Expectations.parsesTo @a sql)

-- | Wraps 'Expectations.rejects' as a standalone 'Spec'.
itRejects :: forall a. (HasCallStack, IsAst a, Show a) => Text -> Spec
itRejects sql =
  describe (Text.unpack sql) $ do
    it "Rejects" (Expectations.rejects @a sql)

-- | Wraps 'Expectations.reportsError' as a standalone 'Spec'.
itReportsError :: forall a. (HasCallStack, IsAst a) => Text -> String -> Spec
itReportsError sql expected =
  describe (Text.unpack sql) $ do
    it ("Reports error: " <> expected) (Expectations.reportsError @a sql expected)

-- | Wraps 'Expectations.parsesWithin' as a standalone 'Spec'.
itParsesWithin :: forall a. (HasCallStack, IsAst a) => Int -> Text -> Spec
itParsesWithin seconds sql =
  describe (Text.unpack sql) $ do
    it ("Parses within " <> show seconds <> " seconds") (Expectations.parsesWithin @a seconds sql)
