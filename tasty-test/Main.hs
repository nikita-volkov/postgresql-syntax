module Main where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import PostgresqlSyntax.Ast
import qualified PostgresqlSyntax.Parsing as Parsing
import qualified PostgresqlSyntax.Rendering as Rendering
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (assert)

main :: IO ()
main =
  defaultMain
    $ testGroup
      ""
      [ testGroup "Parsers"
          $ let testParserOnAllInputs parserName parser inputs =
                  testCase parserName
                    $ forM_ inputs
                    $ \input -> case Parsing.run parser input of
                      Left err -> assertFailure (err <> "\ninput: " <> Text.unpack input)
                      Right _ -> return ()
             in [ testParserOnAllInputs
                    "preparableStmt"
                    Parsing.preparableStmt
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
                    ],
                  testParserOnAllInputs
                    "typename"
                    Parsing.typename
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
                    ],
                  testParserOnAllInputs
                    "sconst"
                    Parsing.sconst
                    [ "'it''s good'",
                      "$$it's good$$",
                      "$x$it's good$x$"
                    ]
                ],
        testGroup "Nesting depth" nestingTests,
        testGroup "Error reporting"
          $ let testParserOnAllInputs parserName parser inputs res =
                  testCase parserName
                    $ forM_ inputs
                    $ \input -> case Parsing.runWithPosError parser input of
                      Left err -> show (NonEmpty.head err) @?= res
                      Right _ -> return ()
             in [ testParserOnAllInputs
                    "Typo in FROM keyword"
                    Parsing.preparableStmt
                    [ "select i :: int8 fom auth.user as u\n\
                      \inner join edgenode.usere_provider as p\n\
                      \on u.id = p.user_id\n\
                      \inner join edgenode.provider_branch as b\n\
                      \on b.provider_fk = p.provider_id"
                    ]
                    "(20,\"offset=20:\\nunexpected space\\nexpecting end of input\\n\")",
                  testParserOnAllInputs
                    "Typo in NOT keyword"
                    Parsing.preparableStmt
                    [ "select i :: int8 from auth.user as u\n\
                      \WHERE u.id IS NO NULL && TRUE"
                    ]
                    "(51,\"offset=51:\\nexpecting white space\\n\")"
                ]
      ]

-- |
-- Regression tests for the parser's behaviour on deeply nested parentheses.
--
-- The parser used to take time exponential in the nesting depth, because
-- several grammar alternatives each parsed the content of a parenthesised
-- group before discovering they did not apply. Every input here is trivial in
-- size; before the fix, all of them ran for longer than a person will wait.
--
-- Each case is given a generous wall-clock budget. The point is not to measure
-- performance — 'nesting-bench' does that — but to fail loudly if the
-- exponential behaviour ever comes back.
nestingTests :: [TestTree]
nestingTests =
  [ parsesWithin "redundant parens, depth 50" 5 Parsing.aExpr
      $ Text.replicate 50 "("
      <> "a + b"
      <> Text.replicate 50 ")",
    parsesWithin "redundant parens around a select, depth 50" 5 Parsing.preparableStmt
      $ "select "
      <> Text.replicate 50 "("
      <> "a + b"
      <> Text.replicate 50 ")",
    parsesWithin "sum of COALESCE terms in two wrapped groups" 5 Parsing.aExpr
      $ let terms off = Text.intercalate " + " ["coalesce(c" <> Text.pack (show (off + i)) <> ", 0)" | i <- [1 .. 24 :: Int]]
         in Text.replicate 6 "(" <> "(" <> terms 0 <> ") - (" <> terms 24 <> ")" <> Text.replicate 6 ")",
    -- The parenthesised sub-select has two possible representations; see the
    -- "Canonical shape" note on 'PostgresqlSyntax.Parsing.selectWithParensBody'.
    testCase "redundant parens around a sub-select are canonicalised" $ do
      let expected =
            WithParensSelectWithParens
              . NoParensSelectWithParens
              <$> Parsing.run Parsing.selectNoParens "select 1"
      Parsing.run Parsing.selectWithParens "((select 1))" @?= expected,
    testCase "OVERLAPS still parses" $ do
      let render = Rendering.toText . Rendering.aExpr
      fmap render (Parsing.run Parsing.aExpr "(1, 2) overlaps (3, 4)")
        @?= Right "(1, 2) OVERLAPS (3, 4)"
      fmap render (Parsing.run Parsing.aExpr "row(1, 2) overlaps row(3, 4)")
        @?= Right "ROW (1, 2) OVERLAPS ROW (3, 4)"
  ]
  where
    parsesWithin name seconds parser input =
      testCase name $ do
        result <- timeout (seconds * 1000000) $ case Parsing.run parser input of
          Left err -> assertFailure (err <> "\ninput: " <> Text.unpack input)
          Right _ -> return ()
        case result of
          Nothing -> assertFailure ("Did not finish parsing within " <> show seconds <> "s")
          Just () -> return ()
