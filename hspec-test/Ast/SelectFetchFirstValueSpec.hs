module Ast.SelectFetchFirstValueSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SelectFetchFirstValue
import Test.Hspec

spec :: Spec
spec = fullSpec @SelectFetchFirstValue
