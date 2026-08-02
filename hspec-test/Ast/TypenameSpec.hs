module Ast.TypenameSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Typename
import PostgresqlSyntax.Settings (nullabilityMarkers)
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Typename
  itSatisfiesArbitrary @Typename
  describe "Parsers" $ do
    itParses @Typename "int4"
    itParses @Typename "int4[][]"
    itParses @Typename "aa array"
    itParses @Typename "DOUBLE PRECISION"
    itParses @Typename "bool"
    itParses @Typename "int2"
    itParses @Typename "int4"
    itParses @Typename "int8"
    itParses @Typename "float4"
    itParses @Typename "float8"
    itParses @Typename "numeric"
    itParses @Typename "char"
    itParses @Typename "text"
    itParses @Typename "bytea"
    itParses @Typename "date"
    itParses @Typename "timestamp"
    itParses @Typename "timestamptz"
    itParses @Typename "time"
    itParses @Typename "timetz"
    itParses @Typename "interval"
    itParses @Typename "uuid"
    itParses @Typename "inet"
    itParses @Typename "json"
    itParses @Typename "jsonb"
  describe "extended" $ do
    itParsesWith @Typename (nullabilityMarkers True) "text?"
    itParsesWith @Typename (nullabilityMarkers True) "text[]?"
    itParsesWith @Typename (nullabilityMarkers True) "text?[]?"
    itParsesWith @Typename (nullabilityMarkers True) "text?[]"
    itRejectsWith @Typename mempty "text?"
    itRejectsWith @Typename mempty "text[]?"
    itRejectsWith @Typename mempty "text?[]?"
    itRejectsWith @Typename mempty "text?[]"
