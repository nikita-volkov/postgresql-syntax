module Ast.SelectWithParensSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectNoParens
import PostgresqlSyntax.Ast.SelectWithParens
import PostgresqlSyntax.IsAst
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectWithParens
  itSatisfiesArbitrary @SelectWithParens
  describe "Nesting depth" $ do
    -- The parenthesised sub-select has two possible representations.
    it "redundant parens around a sub-select are canonicalised"
      $ parse @SelectWithParens "((select 1))"
      `shouldBe` (WithParensSelectWithParens . NoParensSelectWithParens <$> parse @SelectNoParens "select 1")
