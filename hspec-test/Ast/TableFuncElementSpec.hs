module Ast.TableFuncElementSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.TableFuncElement
import Test.Hspec

spec :: Spec
spec = fullSpec @TableFuncElement
