module Ast.SortBySpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SortBy
import PostgresqlSyntax.IsAst
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SortBy
  itSatisfiesArbitrary @SortBy
  describe "Postgres grammar conformance" $ do
    it "sortby" $ do
      let render :: SortBy -> Text
          render = toText mempty
      fmap render (parse @SortBy mempty "a") `shouldBe` Right "a"
      fmap render (parse @SortBy mempty "a asc") `shouldBe` Right "a ASC"
      fmap render (parse @SortBy mempty "a desc nulls last") `shouldBe` Right "a DESC NULLS LAST"
      fmap render (parse @SortBy mempty "a nulls first") `shouldBe` Right "a NULLS FIRST"
      fmap render (parse @SortBy mempty "a using > nulls last") `shouldBe` Right "a USING > NULLS LAST"
      case parse @SortBy mempty "nulls" of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected a parse failure for bare \"nulls\""
