module Ast.TableRefSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.JoinedTable
import PostgresqlSyntax.Ast.TableRef
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @TableRef
  itSatisfiesExtends @TableRef @JoinedTable
  itSatisfiesArbitrary @TableRef
