module Ast.AExprSpec (spec) where

import qualified Data.Text as Text
import Helpers.Specs
import PostgresqlSyntax.Ast.AExpr
import PostgresqlSyntax.IsAst
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @AExpr
  itSatisfiesArbitrary @AExpr
  describe "Postgres grammar conformance" $ do
    -- gram.y:15985,15987 have only @a_expr qual_Op a_expr@ and
    -- @qual_Op a_expr@ — the postfix @a_expr qual_Op@ form was removed
    -- from Postgres in v14.
    describe "rejects postfix operators" $ do
      itRejects @AExpr "1 +#"
      itRejects @AExpr "1 OPERATOR(pg_catalog.+#)"
      itRejects @AExpr "a +#"
  describe "Nesting depth" $ do
    itParsesWithin @AExpr 5 (Text.replicate 50 "(" <> "a + b" <> Text.replicate 50 ")")
    let terms off = Text.intercalate " + " ["coalesce(c" <> Text.pack (show (off + i)) <> ", 0)" | i <- [1 .. 24 :: Int]]
        coalesceSumInput = Text.replicate 6 "(" <> "(" <> terms 0 <> ") - (" <> terms 24 <> ")" <> Text.replicate 6 ")"
    itParsesWithin @AExpr 5 coalesceSumInput
    it "OVERLAPS still parses" $ do
      let render :: AExpr -> Text
          render = toText
      fmap render (parse @AExpr "(1, 2) overlaps (3, 4)") `shouldBe` Right "(1, 2) OVERLAPS (3, 4)"
      fmap render (parse @AExpr "row(1, 2) overlaps row(3, 4)") `shouldBe` Right "ROW (1, 2) OVERLAPS ROW (3, 4)"
