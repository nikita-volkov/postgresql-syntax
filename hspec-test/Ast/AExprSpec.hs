module Ast.AExprSpec (spec) where

import qualified Data.Text as Text
import Helpers.Specs
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.AExpr
import PostgresqlSyntax.Ast.CExpr
import PostgresqlSyntax.Ast.Columnref
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.VerbalExprBinOp
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
          render = toText mempty
      fmap render (parse @AExpr mempty "(1, 2) overlaps (3, 4)") `shouldBe` Right "(1, 2) OVERLAPS (3, 4)"
      fmap render (parse @AExpr mempty "row(1, 2) overlaps row(3, 4)") `shouldBe` Right "ROW (1, 2) OVERLAPS ROW (3, 4)"
  describe "LIKE/ILIKE/SIMILAR TO ESCAPE rhs" $ do
    -- Regression for a bug introduced in 92a4d96: the ESCAPE-clause guard on
    -- the LIKE/ILIKE/SIMILAR TO pattern operand (`d`) was applied whenever an
    -- ESCAPE clause was present at all, instead of only when `d` is itself an
    -- unbounded shape. That spuriously parenthesized bounded operands (a
    -- plain column reference here), producing an extra 'InParensCExpr' layer
    -- on reparse that the original AST never had.
    let render :: AExpr -> Text
        render = toText mempty
        roundtrip sql = fmap render (parse @AExpr mempty sql)
    it "does not add parens around a bounded column-reference rhs" $ do
      roundtrip "a LIKE b ESCAPE 'x'" `shouldBe` Right "a LIKE b ESCAPE 'x'"
    it "does not add parens around a bounded ILIKE rhs" $ do
      roundtrip "a ILIKE b ESCAPE 'x'" `shouldBe` Right "a ILIKE b ESCAPE 'x'"
    it "does not add parens around a bounded SIMILAR TO rhs" $ do
      roundtrip "a SIMILAR TO b ESCAPE 'x'" `shouldBe` Right "a SIMILAR TO b ESCAPE 'x'"
    it "still parenthesizes an unbounded (nested, escape-less LIKE) rhs" $ do
      -- Constructed directly rather than by parsing a string: parsing
      -- "a LIKE b LIKE c ESCAPE 'x'" greedily binds the ESCAPE to the
      -- innermost LIKE (its own @d@ operand is unrestricted @a_expr@), so it
      -- can't reach the shape below by parsing alone. The shape is real
      -- though: an unbounded, escape-less nested 'VerbalExprBinOpAExpr' as
      -- the rhs of an outer production that itself has an ESCAPE clause. Left
      -- unparenthesized, rendering it bare would let a reparse of the outer
      -- ESCAPE bind to the inner LIKE instead, changing the AST.
      -- Reparsing always wraps a parenthesized operand in an explicit
      -- 'InParensCExpr' (parens are themselves a production), so the
      -- fixpoint of render/parse is the parenthesized form, not the bare
      -- @outer@ below — that's expected and matches how
      -- 'PostgresqlSyntax.Ast.AExpr.Qc.Arbitrary'\'s generator pre-wraps
      -- this same shape. What matters here is that the parens are present at
      -- all, and that the fixpoint is stable.
      let colref name = CExprAExpr (ColumnrefCExpr (Columnref (UnquotedIdent name) Nothing))
          innerD = VerbalExprBinOpAExpr (colref "b") False LikeVerbalExprBinOp (colref "c") Nothing
          outer = VerbalExprBinOpAExpr (colref "a") False LikeVerbalExprBinOp innerD (Just (colref "e"))
          wrappedOuter = VerbalExprBinOpAExpr (colref "a") False LikeVerbalExprBinOp (CExprAExpr (InParensCExpr innerD Nothing)) (Just (colref "e"))
          sql = render outer
      sql `shouldBe` "a LIKE (b LIKE c) ESCAPE e"
      parse @AExpr mempty sql `shouldBe` Right wrappedOuter
      fmap render (parse @AExpr mempty sql) `shouldBe` Right sql
