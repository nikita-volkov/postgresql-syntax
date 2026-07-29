module Ast.WhereOrCurrentClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.WhereOrCurrentClause
import Test.Hspec

spec :: Spec
spec = fullSpec @WhereOrCurrentClause
