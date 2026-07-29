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

  -- Grammar constructs pinned directly against
  -- @references/gram.y@ at the commit recorded in AGENTS.md.
  describe "Postgres grammar conformance" $ do
    -- gram.y:15985,15987 have only @a_expr qual_Op a_expr@ and
    -- @qual_Op a_expr@ — the postfix @a_expr qual_Op@ form was removed
    -- from Postgres in v14.
    it "rejects postfix operators" $ do
      rejects @AExpr "1 +#"
      rejects @AExpr "1 OPERATOR(pg_catalog.+#)"
      rejects @AExpr "a +#"

    -- gram.y:17567 frame_bound. UNBOUNDED is an unreserved keyword, so
    -- @UNBOUNDED PRECEDING@ is ambiguous with @a_expr PRECEDING@ where
    -- the a_expr is a column named "unbounded"; gram.y:915 resolves it by
    -- giving UNBOUNDED lower precedence than PRECEDING, i.e. the keyword
    -- reading wins and the column reading needs quoting.
    it "frame_bound" $ do
      let render :: FrameBound -> Text
          render = toText
      fmap render (parse @FrameBound "unbounded preceding") `shouldBe` Right "UNBOUNDED PRECEDING"
      fmap render (parse @FrameBound "unbounded following") `shouldBe` Right "UNBOUNDED FOLLOWING"
      fmap render (parse @FrameBound "current row") `shouldBe` Right "CURRENT ROW"
      fmap render (parse @FrameBound "1 preceding") `shouldBe` Right "1 PRECEDING"
      fmap render (parse @FrameBound "a following") `shouldBe` Right "a FOLLOWING"
      fmap render (parse @FrameBound "\"unbounded\" preceding") `shouldBe` Right "\"unbounded\" PRECEDING"

    -- gram.y:17428 window_specification: the sort clause and the
    -- partition clause are both followed by opt_frame_clause, whose
    -- leading keywords (RANGE/ROWS/GROUPS, kwlist.h:375,408,201) are
    -- unreserved and therefore also legal ColIds.
    it "window_specification terminators are not swallowed by the expression" $ do
      parsesTo @WindowSpecification "(order by a rows unbounded preceding)"
      parsesTo @WindowSpecification "(order by a range unbounded preceding)"
      parsesTo @WindowSpecification "(partition by a groups unbounded preceding)"
      parsesTo @WindowSpecification "(partition by a order by b rows 1 preceding)"

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

parsesTo :: forall a. (HasCallStack, IsAst a) => Text -> Expectation
parsesTo input =
  case parse @a input of
    Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
    Right _ -> pure ()

-- | Asserts that the input is *not* accepted. Used to pin grammar
-- constructs that Postgres itself rejects.
rejects :: forall a. (HasCallStack, IsAst a, Show a) => Text -> Expectation
rejects input =
  case parse @a input of
    Left _ -> pure ()
    Right a ->
      expectationFailure
        ("expected a parse failure\ninput: " <> Text.unpack input <> "\nparsed: " <> show a)

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
