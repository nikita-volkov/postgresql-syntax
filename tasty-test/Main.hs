module Main where

import Prelude hiding (assert)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified PostgresqlSyntax.Ast as Ast
import qualified PostgresqlSyntax.Parsing as Parsing
import qualified PostgresqlSyntax.Rendering as Rendering
import qualified Data.Text as Text


main = defaultMain $ testGroup "" [
    testGroup "Parsers" $ let
      testParserOnAllInputs parserName parser inputs =
        testCase parserName $ forM_ inputs $ \ input -> case Parsing.run parser input of
          Left err -> assertFailure (err <> "\ninput: " <> Text.unpack input)
          Right _ -> return ()
      testParserOnEachInput parserName parser inputs =
        testGroup parserName $ flip fmap inputs $ \ input ->
        testCase (Text.unpack input) $ case Parsing.run parser input of
          Left err -> assertFailure err
          Right _ -> return ()
      in [
          testParserOnAllInputs "preparableStmt" Parsing.preparableStmt [
              "select i :: int8 from auth.user as u\n\
              \inner join edgenode.usere_provider as p\n\
              \on u.id = p.user_id\n\
              \inner join edgenode.provider_branch as b\n\
              \on b.provider_fk = p.provider_id"
            ],
          testParserOnAllInputs "typename" Parsing.typename [
              "aa array",
              "DOUBLE PRECISION",
              "bool", "int2", "int4", "int8", "float4", "float8", "numeric", "char", "text", "bytea", "date", "timestamp", "timestamptz", "time", "timetz", "interval", "uuid", "inet", "json", "jsonb"
            ]
        ]
  ]
