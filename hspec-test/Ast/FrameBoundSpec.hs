module Ast.FrameBoundSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FrameBound
import PostgresqlSyntax.IsAst
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  fullSpec @FrameBound
  describe "Postgres grammar conformance" $ do
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
