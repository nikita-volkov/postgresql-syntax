{-# LANGUAGE AllowAmbiguousTypes #-}

module Cases (spec) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import PostgresqlSyntax
import Prelude
import Test.Hspec

-- | Example-based cases: parser acceptance on sample inputs, the
-- nesting-depth regressions, and error-reporting checks. The counterpart
-- to the property suite in "Properties".
spec :: Spec
spec = do
  describe "Parsers" $ do
    it "preparableStmt"
      $ forM_
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
        (parsesTo @PreparableStmt)
    it "typename"
      $ forM_
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
        (parsesTo @Typename)
    it "sconst"
      $ forM_
        [ "'it''s good'",
          "$$it's good$$",
          "$x$it's good$x$"
        ]
        (parsesTo @Sconst)

  describe "Nesting depth" $ do
    it "redundant parens, depth 50"
      $ parsesWithin @AExpr 5 (Text.replicate 50 "(" <> "a + b" <> Text.replicate 50 ")")
    it "redundant parens around a select, depth 50"
      $ parsesWithin @PreparableStmt 5 ("select " <> Text.replicate 50 "(" <> "a + b" <> Text.replicate 50 ")")
    it "sum of COALESCE terms in two wrapped groups"
      $ let terms off = Text.intercalate " + " ["coalesce(c" <> Text.pack (show (off + i)) <> ", 0)" | i <- [1 .. 24 :: Int]]
            coalesceSumInput = Text.replicate 6 "(" <> "(" <> terms 0 <> ") - (" <> terms 24 <> ")" <> Text.replicate 6 ")"
         in parsesWithin @AExpr 5 coalesceSumInput
    -- The parenthesised sub-select has two possible representations.
    it "redundant parens around a sub-select are canonicalised"
      $ parse @SelectWithParens "((select 1))"
      `shouldBe` (WithParensSelectWithParens . NoParensSelectWithParens <$> parse @SelectNoParens "select 1")
    it "OVERLAPS still parses" $ do
      let render :: AExpr -> Text
          render = toText
      fmap render (parse @AExpr "(1, 2) overlaps (3, 4)") `shouldBe` Right "(1, 2) OVERLAPS (3, 4)"
      fmap render (parse @AExpr "row(1, 2) overlaps row(3, 4)") `shouldBe` Right "ROW (1, 2) OVERLAPS ROW (3, 4)"

  describe "Error reporting" $ do
    it "Typo in FROM keyword"
      $ reportsError @PreparableStmt
        "select i :: int8 fom auth.user as u\n\
        \inner join edgenode.usere_provider as p\n\
        \on u.id = p.user_id\n\
        \inner join edgenode.provider_branch as b\n\
        \on b.provider_fk = p.provider_id"
        "(21,\"offset=21:\\nunexpected 'a'\\nexpecting end of input or white space\\n\")"
    it "Typo in NOT keyword"
      $ reportsError @PreparableStmt
        "select i :: int8 from auth.user as u\n\
        \WHERE u.id IS NO NULL && TRUE"
        "(51,\"offset=51:\\nexpecting white space\\n\")"

-- * Example-based parse helpers

parsesTo :: forall a. (IsAst a) => Text -> Expectation
parsesTo input =
  case parse @a input of
    Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
    Right _ -> pure ()

reportsError :: forall a. (IsAst a) => Text -> String -> Expectation
reportsError input expected =
  case parseWithPosError @a input of
    Left err -> show (NonEmpty.head err) `shouldBe` expected
    Right _ -> expectationFailure "expected a parse error, but it succeeded"

parsesWithin :: forall a. (IsAst a) => Int -> Text -> Expectation
parsesWithin seconds input = do
  result <-
    timeout (seconds * 1000000)
      $ case parse @a input of
        Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
        Right _ -> pure ()
  case result of
    Nothing -> expectationFailure ("Did not finish parsing within " <> show seconds <> "s")
    Just () -> pure ()
