module Ast.InsertRestSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.InsertRest
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @InsertRest
  itSatisfiesArbitrary @InsertRest
  -- https://github.com/nikita-volkov/postgresql-syntax/issues/35
  itParses @InsertRest "(values (default))"
  itParses @InsertRest "(select 1)"
  itParses @InsertRest "(values) values (1)"
