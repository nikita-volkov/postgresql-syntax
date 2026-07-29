module Ast.TypenameSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Typename
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  fullSpec @Typename
  describe "Parsers" $ do
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
