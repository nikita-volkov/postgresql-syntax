module Ast.IndexElemSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.IndexElem
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @IndexElem
  itSatisfiesArbitrary @IndexElem
  describe "Postgres grammar conformance" $ do
    -- gram.y:8558 index_elem: ColId index_elem_options, and gram.y:8596
    -- opt_nulls_order. index_elem_options' operator-class name
    -- (opt_qualified_name, gram.y:8525) is a bare ColId, so it is
    -- directly ambiguous with the unreserved NULLS that follows it.
    it "index_elem" $ do
      let render :: IndexElem -> Text
          render = toText mempty
      fmap render (parse @IndexElem mempty "a") `shouldBe` Right "a"
      fmap render (parse @IndexElem mempty "a nulls first") `shouldBe` Right "a NULLS FIRST"
      fmap render (parse @IndexElem mempty "a text_ops nulls first") `shouldBe` Right "a text_ops NULLS FIRST"
      fmap render (parse @IndexElem mempty "a collate \"C\" text_ops desc") `shouldBe` Right "a COLLATE \"C\" text_ops DESC"
