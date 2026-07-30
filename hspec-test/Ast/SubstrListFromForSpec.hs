module Ast.SubstrListFromForSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SubstrListFromFor
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SubstrListFromFor
  itSatisfiesArbitrary @SubstrListFromFor
