module Ast.OnConflictSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.OnConflict
import Test.Hspec

spec :: Spec
spec = fullSpec @OnConflict
