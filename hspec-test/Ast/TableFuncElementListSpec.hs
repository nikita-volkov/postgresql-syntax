module Ast.TableFuncElementListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.TableFuncElementList
import Test.Hspec

spec :: Spec
spec = fullSpec @TableFuncElementList
