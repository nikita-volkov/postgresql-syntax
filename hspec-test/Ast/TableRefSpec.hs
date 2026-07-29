module Ast.TableRefSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TableRef
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @TableRef
  itSatisfiesArbitrary @TableRef
