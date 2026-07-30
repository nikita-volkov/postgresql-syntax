module Ast.TableRefSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TableRef
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @TableRef
  itSatisfiesArbitrary @TableRef
