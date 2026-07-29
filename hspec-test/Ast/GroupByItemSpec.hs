module Ast.GroupByItemSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.GroupByItem
import Test.Hspec

spec :: Spec
spec = fullSpec @GroupByItem
