module Ast.SimpleSelectSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.QualifiedName
import PostgresqlSyntax.Ast.RelationExpr
import PostgresqlSyntax.Ast.SelectBinOp
import PostgresqlSyntax.Ast.SelectClause
import PostgresqlSyntax.Ast.SimpleSelect
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SimpleSelect
  itSatisfiesCanonicalizes @SimpleSelect
  itSatisfiesArbitrary @SimpleSelect
  describe "Precedence" precedenceSpec

-- |
-- Reproduces the three parse-shape checks from
-- <https://github.com/nikita-volkov/postgresql-syntax/issues/30> against
-- @gram.y@'s @%left UNION EXCEPT@ \/ @%left INTERSECT@ declarations
-- (gram.y:813-814): @INTERSECT@ binds tighter than @UNION@\/@EXCEPT@, and
-- all three are left-associative among themselves.
precedenceSpec :: Spec
precedenceSpec = do
  it "a EXCEPT b EXCEPT c nests left"
    $ parse mempty "TABLE a EXCEPT TABLE b EXCEPT TABLE c"
    `shouldBe` Right (bin ExceptSelectBinOp (bin ExceptSelectBinOp (table "a") (table "b")) (table "c"))
  it "a INTERSECT b UNION c roots at UNION"
    $ parse mempty "TABLE a INTERSECT TABLE b UNION TABLE c"
    `shouldBe` Right (bin UnionSelectBinOp (bin IntersectSelectBinOp (table "a") (table "b")) (table "c"))
  it "a UNION b INTERSECT c is a UNION (b INTERSECT c)"
    $ parse mempty "TABLE a UNION TABLE b INTERSECT TABLE c"
    `shouldBe` Right (bin UnionSelectBinOp (table "a") (bin IntersectSelectBinOp (table "b") (table "c")))
  it "a INTERSECT b INTERSECT c nests left, not right (issue #34)"
    $ parse mempty "TABLE a INTERSECT TABLE b INTERSECT TABLE c"
    `shouldBe` Right (bin IntersectSelectBinOp (bin IntersectSelectBinOp (table "a") (table "b")) (table "c"))
  it "a INTERSECT b INTERSECT c INTERSECT d nests fully left (issue #34)"
    $ parse mempty "TABLE a INTERSECT TABLE b INTERSECT TABLE c INTERSECT TABLE d"
    `shouldBe` Right (bin IntersectSelectBinOp (bin IntersectSelectBinOp (bin IntersectSelectBinOp (table "a") (table "b")) (table "c")) (table "d"))
  where
    table name = TableSimpleSelect (SimpleRelationExpr (SimpleQualifiedName (UnquotedIdent name)) False)
    bin op lhs rhs = BinSimpleSelect op (SimpleSelectSelectClause lhs) Nothing (SimpleSelectSelectClause rhs)
