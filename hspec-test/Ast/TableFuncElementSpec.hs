module Ast.TableFuncElementSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TableFuncElement
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @TableFuncElement
  itSatisfiesArbitrary @TableFuncElement
