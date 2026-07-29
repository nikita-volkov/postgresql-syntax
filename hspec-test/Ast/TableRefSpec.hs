module Ast.TableRefSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.TableRef
import Test.Hspec

spec :: Spec
spec = fullSpec @TableRef
