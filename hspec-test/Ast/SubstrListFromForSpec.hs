module Ast.SubstrListFromForSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SubstrListFromFor
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SubstrListFromFor
  itSatisfiesArbitrary @SubstrListFromFor
