module Ast.FuncAliasClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FuncAliasClause
import Test.Hspec

spec :: Spec
spec = fullSpec @FuncAliasClause
