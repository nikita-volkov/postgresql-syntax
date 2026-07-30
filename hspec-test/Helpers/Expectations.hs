{-# LANGUAGE AllowAmbiguousTypes #-}

module Helpers.Expectations
  ( parsesTo,
    rejects,
    reportsError,
    parsesWithin,
    parsesToWith,
    rejectsWith,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Settings (Settings)
import Prelude
import Test.Hspec

parsesTo :: forall a. (HasCallStack, IsAst a) => Text -> Expectation
parsesTo input =
  case parse @a mempty input of
    Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
    Right _ -> pure ()

rejects :: forall a. (HasCallStack, IsAst a, Show a) => Text -> Expectation
rejects input =
  case parse @a mempty input of
    Left _ -> pure ()
    Right a ->
      expectationFailure
        ("expected a parse failure\ninput: " <> Text.unpack input <> "\nparsed: " <> show a)

reportsError :: forall a. (HasCallStack, IsAst a) => Text -> String -> Expectation
reportsError input expected =
  case parseWithPosError @a mempty input of
    Left err -> show (NonEmpty.head err) `shouldBe` expected
    Right _ -> expectationFailure "expected a parse error, but it succeeded"

parsesWithin :: forall a. (HasCallStack, IsAst a) => Int -> Text -> Expectation
parsesWithin seconds input = do
  result <-
    timeout (seconds * 1000000)
      $ case parse @a mempty input of
        Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
        Right _ -> pure ()
  case result of
    Nothing -> expectationFailure ("Did not finish parsing within " <> show seconds <> "s")
    Just () -> pure ()

-- | Like 'parsesTo' but with a non-default 'Settings'.
parsesToWith :: forall a. (HasCallStack, IsAst a) => Settings -> Text -> Expectation
parsesToWith settings input =
  case parse @a settings input of
    Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
    Right _ -> pure ()

-- | Like 'rejects' but with a non-default 'Settings'.
rejectsWith :: forall a. (HasCallStack, IsAst a, Show a) => Settings -> Text -> Expectation
rejectsWith settings input =
  case parse @a settings input of
    Left _ -> pure ()
    Right a ->
      expectationFailure
        ("expected a parse failure\ninput: " <> Text.unpack input <> "\nparsed: " <> show a)
