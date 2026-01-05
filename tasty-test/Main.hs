module Main where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified PostgresqlSyntax.Parsing as Parsing
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
                      \on b.provider_fk = p.provider_id"
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
                    "(53,\"offset=53:\\nexpecting white space\\n\")"
                ]
      ]
