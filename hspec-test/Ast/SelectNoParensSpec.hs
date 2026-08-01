module Ast.SelectNoParensSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectNoParens
import PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectNoParens
  itSatisfiesRefines @SelectWithParens @SelectNoParens
  itSatisfiesArbitrary @SelectNoParens
