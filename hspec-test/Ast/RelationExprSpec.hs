module Ast.RelationExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.RelationExpr
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @RelationExpr
  itSatisfiesArbitrary @RelationExpr
