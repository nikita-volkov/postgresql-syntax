module Ast.RelationExprOptAliasSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.RelationExprOptAlias
import Test.Hspec

spec :: Spec
spec = fullSpec @RelationExprOptAlias
