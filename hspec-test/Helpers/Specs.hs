{-# LANGUAGE AllowAmbiguousTypes #-}

module Helpers.Specs
  ( itSatisfiesIsAst,
    itSatisfiesArbitrary,
    itParses,
    itRejects,
    itReportsError,
    itReportsSourcePosError,
    itParsesWithin,

    -- * Settings-aware variants
    itParsesWith,
    itRejectsWith,
  )
where

import qualified Data.Text as Text
import qualified Helpers.Expectations as Expectations
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Settings (Settings)
import Prelude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, (===))
import qualified Test.QuickCheck as Qc

itSatisfiesIsAst :: forall a. (IsAst a, Eq a, Show a, Qc.Arbitrary a) => Spec
itSatisfiesIsAst =
  describe "IsAst" $ do
    prop "Roundtrips" $ \(a :: a) ->
      let sql = toText mempty a
       in case parse mempty sql of
            Left err ->
              counterexample ("rendered: " <> Text.unpack sql <> "\nparse failed: " <> Text.unpack err) False
            Right a' ->
              counterexample
                ("rendered: " <> Text.unpack sql <> "\nrestored: " <> Text.unpack (toText mempty a'))
                (a' === a)

itSatisfiesArbitrary :: forall a. (IsAst a, Show a, Qc.Arbitrary a) => Spec
itSatisfiesArbitrary =
  describe "Arbitrary" $ do
    prop "Terminates at size 0" terminatesAtZero
    prop "Grows boundedly" growsBounded
  where
    terminatesAtZero =
      Qc.forAll (Qc.resize 0 (Qc.arbitrary @a)) $ \x ->
        let sql = toText mempty x
            len = Text.length sql
         in Qc.counterexample
              ("rendered " <> show len <> " chars at size 0 (max " <> show zeroSizeMaxLen <> ")" <> "\n" <> Text.unpack sql)
              (len <= zeroSizeMaxLen)
      where
        zeroSizeMaxLen = 500 :: Int

    growsBounded =
      Qc.forAll (Qc.resize maxGenSize (Qc.arbitrary @a)) $ \x ->
        let sql = toText mempty x
            len = Text.length sql
         in Qc.counterexample
              ("rendered " <> show len <> " chars at size " <> show maxGenSize <> " (max " <> show maxGenSizeMaxLen <> ")")
              (len <= maxGenSizeMaxLen)
      where
        maxGenSize = 100 :: Int
        maxGenSizeMaxLen = 1000000 :: Int

itParses :: forall a. (HasCallStack, IsAst a) => Text -> Spec
itParses sql =
  describe (Text.unpack sql) $ do
    it "Parses" (Expectations.parsesTo @a sql)

itRejects :: forall a. (HasCallStack, IsAst a, Show a) => Text -> Spec
itRejects sql =
  describe (Text.unpack sql) $ do
    it "Rejects" (Expectations.rejects @a sql)

itReportsError :: forall a. (HasCallStack, IsAst a) => Text -> String -> Spec
itReportsError sql expected =
  describe (Text.unpack sql) $ do
    it ("Reports error: " <> expected) (Expectations.reportsError @a sql expected)

-- | Like 'itReportsError' but checks 'PostgresqlSyntax.Algebra.parseWithSourcePosError''s
-- 'Text.Megaparsec.SourcePos'-based errors instead.
itReportsSourcePosError :: forall a. (HasCallStack, IsAst a) => Text -> String -> Spec
itReportsSourcePosError sql expected =
  describe (Text.unpack sql) $ do
    it ("Reports error: " <> expected) (Expectations.reportsSourcePosError @a sql expected)

itParsesWithin :: forall a. (HasCallStack, IsAst a) => Int -> Text -> Spec
itParsesWithin seconds sql =
  describe (Text.unpack sql) $ do
    it ("Parses within " <> show seconds <> " seconds") (Expectations.parsesWithin @a seconds sql)

-- | Wraps 'Expectations.parsesToWith' as a standalone 'Spec'.
itParsesWith :: forall a. (HasCallStack, IsAst a) => Settings -> Text -> Spec
itParsesWith settings sql =
  describe (Text.unpack sql) $ do
    it "Parses" (Expectations.parsesToWith @a settings sql)

-- | Wraps 'Expectations.rejectsWith' as a standalone 'Spec'.
itRejectsWith :: forall a. (HasCallStack, IsAst a, Show a) => Settings -> Text -> Spec
itRejectsWith settings sql =
  describe (Text.unpack sql) $ do
    it "Rejects" (Expectations.rejectsWith @a settings sql)
