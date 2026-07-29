module Ast.RelationExprSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.RelationExpr
import Test.Hspec

spec :: Spec
spec = fullSpec @RelationExpr
