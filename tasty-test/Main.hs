module Main where

import qualified Data.Text as Text
import qualified PostgresqlSyntax.Parsing as Parsing
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec.Pos (sourcePosPretty)
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
        testGroup "Reserved keyword as identifier error"
          $ let testParserOnAllInputs parserName parser inputs res =
                  testCase parserName
                    $ forM_ inputs
                    $ \input -> case Parsing.run parser input of
                      Left err -> err @?= res
                      Right _ -> assertFailure ("Expected a parse failure but succeeded for input: " <> Text.unpack input)
             in [ testParserOnAllInputs
                    "preparableStmt"
                    Parsing.preparableStmt
                    [ "SELECT id FROM as"
                    ]
                    "1:18:\n  |\n1 | SELECT id FROM as\n  |                  ^\nReserved keyword \"as\" used as an identifier. If that's what you intend, you have to wrap it in double quotes.\n"
                ],
        testGroup "Error reporting"
          $ let testParserOnAllInputs parserName parser inputs res =
                  testCase parserName
                    $ forM_ inputs
                    $ \input -> case Parsing.runWithPosError parser input of
                      Left err ->
                        let formattedErrList = (\(p, m) -> sourcePosPretty p <> " " <> m) <$> err
                         in res `elem` formattedErrList @? "Missing error message " <> res <> " in " <> show formattedErrList
                      Right _ -> assertFailure ("Expected a parse failure but succeeded for input: " <> Text.unpack input)
             in [ testParserOnAllInputs
                    "Typo in FROM keyword"
                    Parsing.preparableStmt
                    ["select u.id :: int8 fom auth.user as u"]
                    "1:24 unexpected space\nexpecting end of input\n",
                  testParserOnAllInputs
                    "Typo in select keyword"
                    Parsing.preparableStmt
                    ["SLECT id FROM qsdqsd"]
                    "1:1 unexpected \"slect\"\nexpecting \"call\", \"delete\", \"insert\", \"select\", \"table\", \"update\", \"values\", \"with\", or '('\n",
                  testParserOnAllInputs
                    "Typo in FROM keyword with multiple joins"
                    Parsing.preparableStmt
                    [ "select i :: int8 fom auth.user as u\n\
                      \inner join edgenode.usere_provider as p\n\
                      \on u.id = p.user_id\n\
                      \inner join edgenode.provider_branch as b\n\
                      \on b.provider_fk = p.provider_id"
                    ]
                    "1:21 unexpected space\nexpecting end of input\n",
                  testParserOnAllInputs
                    "Typo in NOT keyword"
                    Parsing.preparableStmt
                    [ "select i :: int8 from auth.user as u\n\
                      \WHERE u.id IS NO NULL && TRUE"
                    ]
                    "2:15 unexpected \"no\"\nexpecting \"distinct\", \"document\", \"false\", \"not\", \"null\", \"of\", \"true\", \"unknown\", or white space\n"
                ]
      ]
