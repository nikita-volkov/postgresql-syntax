module Ast.WindowSpecificationSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WindowSpecification
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @WindowSpecification
  itSatisfiesArbitrary @WindowSpecification
  describe "Postgres grammar conformance" $ do
    -- gram.y:17428 window_specification: the sort clause and the
    -- partition clause are both followed by opt_frame_clause, whose
    -- leading keywords (RANGE/ROWS/GROUPS, kwlist.h:375,408,201) are
    -- unreserved and therefore also legal ColIds.
    itParses @WindowSpecification "(order by a rows unbounded preceding)"
    itParses @WindowSpecification "(order by a range unbounded preceding)"
    itParses @WindowSpecification "(partition by a groups unbounded preceding)"
    itParses @WindowSpecification "(partition by a order by b rows 1 preceding)"
