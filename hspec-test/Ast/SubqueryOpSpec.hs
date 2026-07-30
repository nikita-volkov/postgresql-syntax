module Ast.SubqueryOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SubqueryOp
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SubqueryOp
  itSatisfiesArbitrary @SubqueryOp
