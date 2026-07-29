module Ast.AliasClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.AliasClause
import Test.Hspec

spec :: Spec
spec = fullSpec @AliasClause
