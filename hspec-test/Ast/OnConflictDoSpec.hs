module Ast.OnConflictDoSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.OnConflictDo
import Test.Hspec

spec :: Spec
spec = fullSpec @OnConflictDo
