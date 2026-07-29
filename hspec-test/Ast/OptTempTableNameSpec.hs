module Ast.OptTempTableNameSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OptTempTableName
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @OptTempTableName
  itSatisfiesArbitrary @OptTempTableName
