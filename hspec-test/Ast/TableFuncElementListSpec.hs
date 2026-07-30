module Ast.TableFuncElementListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TableFuncElementList
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @TableFuncElementList
  itSatisfiesArbitrary @TableFuncElementList
