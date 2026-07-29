module Ast.WindowSpecificationSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.WindowSpecification
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  fullSpec @WindowSpecification
  describe "Postgres grammar conformance" $ do
    -- gram.y:17428 window_specification: the sort clause and the
    -- partition clause are both followed by opt_frame_clause, whose
    -- leading keywords (RANGE/ROWS/GROUPS, kwlist.h:375,408,201) are
    -- unreserved and therefore also legal ColIds.
    it "window_specification terminators are not swallowed by the expression" $ do
      parsesTo @WindowSpecification "(order by a rows unbounded preceding)"
      parsesTo @WindowSpecification "(order by a range unbounded preceding)"
      parsesTo @WindowSpecification "(partition by a groups unbounded preceding)"
      parsesTo @WindowSpecification "(partition by a order by b rows 1 preceding)"
