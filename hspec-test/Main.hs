{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import PostgresqlSyntax (IsAst, run, runWithPosError, toText)
import PostgresqlSyntax.Ast
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, counterexample, withNumTests, (===))
import Prelude hiding (assert)

main :: IO ()
main = hspec $ do
  describe "Round-trip parse/render" $ do
    prop "Typename" (roundTrip @Typename 10000)
    prop "TableRef" (roundTrip @TableRef 10000)
    prop "AExpr" (roundTrip @AExpr 60000)
    prop "PreparableStmt" (roundTrip @PreparableStmt 30000)

  describe "Parsers" $ do
    it "preparableStmt" $ forM_ preparableStmtInputs (parsesTo @PreparableStmt)
    it "typename" $ forM_ typenameInputs (parsesTo @Typename)
    it "sconst" $ forM_ sconstInputs (parsesTo @Sconst)

  describe "Nesting depth" nestingSpec

  describe "Error reporting" $ do
    it "Typo in FROM keyword"
      $ reportsError @PreparableStmt fromTypoInput "(20,\"offset=20:\\nunexpected space\\nexpecting end of input\\n\")"
    it "Typo in NOT keyword"
      $ reportsError @PreparableStmt notTypoInput "(51,\"offset=51:\\nexpecting white space\\n\")"

-- * Round-trip property

roundTrip :: (IsAst a, Eq a, Show a) => Int -> a -> Property
roundTrip n a = withNumTests n $ counterexample (Text.unpack sql) (run sql === Right a)
  where
    sql = toText a

-- * Example-based parse helpers

parsesTo :: forall a. (IsAst a) => Text -> Expectation
parsesTo input =
  case run @a input of
    Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
    Right _ -> pure ()

reportsError :: forall a. (IsAst a) => Text -> String -> Expectation
reportsError input expected =
  case runWithPosError @a input of
    Left err -> show (NonEmpty.head err) `shouldBe` expected
    Right _ -> expectationFailure "expected a parse error, but it succeeded"

-- * Nesting-depth regressions

--
-- The parser used to take time exponential in the nesting depth, because
-- several grammar alternatives each parsed the content of a parenthesised
-- group before discovering they did not apply. Every input here is trivial in
-- size; before the fix, all of them ran for longer than a person will wait.
--
-- Each case is given a generous wall-clock budget. The point is not to measure
-- performance — 'nesting-bench' does that — but to fail loudly if the
-- exponential behaviour ever comes back.
nestingSpec :: Spec
nestingSpec = do
  it "redundant parens, depth 50"
    $ parsesWithin @AExpr 5 (Text.replicate 50 "(" <> "a + b" <> Text.replicate 50 ")")
  it "redundant parens around a select, depth 50"
    $ parsesWithin @PreparableStmt 5 ("select " <> Text.replicate 50 "(" <> "a + b" <> Text.replicate 50 ")")
  it "sum of COALESCE terms in two wrapped groups"
    $ parsesWithin @AExpr 5 coalesceSumInput
  -- The parenthesised sub-select has two possible representations.
  it "redundant parens around a sub-select are canonicalised"
    $ run @SelectWithParens "((select 1))"
    `shouldBe` (WithParensSelectWithParens . NoParensSelectWithParens <$> run @SelectNoParens "select 1")
  it "OVERLAPS still parses" $ do
    let render :: AExpr -> Text
        render = toText
    fmap render (run @AExpr "(1, 2) overlaps (3, 4)") `shouldBe` Right "(1, 2) OVERLAPS (3, 4)"
    fmap render (run @AExpr "row(1, 2) overlaps row(3, 4)") `shouldBe` Right "ROW (1, 2) OVERLAPS ROW (3, 4)"

parsesWithin :: forall a. (IsAst a) => Int -> Text -> Expectation
parsesWithin seconds input = do
  result <-
    timeout (seconds * 1000000)
      $ case run @a input of
        Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
        Right _ -> pure ()
  case result of
    Nothing -> expectationFailure ("Did not finish parsing within " <> show seconds <> "s")
    Just () -> pure ()

-- * Inputs

preparableStmtInputs :: [Text]
preparableStmtInputs =
  [ "select i :: int8 from auth.user as u\n\
    \inner join edgenode.usere_provider as p\n\
    \on u.id = p.user_id\n\
    \inner join edgenode.provider_branch as b\n\
    \on b.provider_fk = p.provider_id",
    -- FOR locking clause before LIMIT (PostgreSQL accepts both orderings)
    "select * from items for update limit 1",
    "select * from items limit 1 for update",
    "select * from items for share limit 10",
    "select * from items for no key update limit 1",
    "select * from items for key share limit 1",
    "select * from items for update of items nowait limit 1",
    "select * from items for update skip locked limit 1",
    "select * from items order by id for update limit 1",
    "select * from items for update offset 5 limit 10"
  ]

typenameInputs :: [Text]
typenameInputs =
  [ "int4[]",
    "int4[][]",
    "int4?[]",
    "int4?[]?",
    "aa array",
    "DOUBLE PRECISION",
    "bool",
    "int2",
    "int4",
    "int8",
    "float4",
    "float8",
    "numeric",
    "char",
    "text",
    "bytea",
    "date",
    "timestamp",
    "timestamptz",
    "time",
    "timetz",
    "interval",
    "uuid",
    "inet",
    "json",
    "jsonb"
  ]

sconstInputs :: [Text]
sconstInputs =
  [ "'it''s good'",
    "$$it's good$$",
    "$x$it's good$x$"
  ]

coalesceSumInput :: Text
coalesceSumInput =
  let terms off = Text.intercalate " + " ["coalesce(c" <> Text.pack (show (off + i)) <> ", 0)" | i <- [1 .. 24 :: Int]]
   in Text.replicate 6 "(" <> "(" <> terms 0 <> ") - (" <> terms 24 <> ")" <> Text.replicate 6 ")"

fromTypoInput :: Text
fromTypoInput =
  "select i :: int8 fom auth.user as u\n\
  \inner join edgenode.usere_provider as p\n\
  \on u.id = p.user_id\n\
  \inner join edgenode.provider_branch as b\n\
  \on b.provider_fk = p.provider_id"

notTypoInput :: Text
notTypoInput =
  "select i :: int8 from auth.user as u\n\
  \WHERE u.id IS NO NULL && TRUE"
