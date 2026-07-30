module Ast.NameListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.NameList
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @NameList
  itSatisfiesArbitrary @NameList
