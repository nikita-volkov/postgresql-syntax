module Ast.SubstrListFromForSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SubstrListFromFor
import Test.Hspec

spec :: Spec
spec = fullSpec @SubstrListFromFor
