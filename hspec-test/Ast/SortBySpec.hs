module Ast.SortBySpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SortBy
import Test.Hspec

spec :: Spec
spec = fullSpec @SortBy
