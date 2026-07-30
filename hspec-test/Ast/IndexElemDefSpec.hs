module Ast.IndexElemDefSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.IndexElemDef
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @IndexElemDef
  itSatisfiesArbitrary @IndexElemDef
