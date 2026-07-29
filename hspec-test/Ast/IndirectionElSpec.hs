module Ast.IndirectionElSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.IndirectionEl
import Test.Hspec

spec :: Spec
spec = fullSpec @IndirectionEl
