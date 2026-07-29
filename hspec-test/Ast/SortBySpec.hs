module Ast.SortBySpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SortBy
import PostgresqlSyntax.IsAst
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SortBy
  itSatisfiesArbitrary @SortBy
  describe "Postgres grammar conformance" $ do
    -- gram.y:8596 opt_nulls_order and gram.y:14056 sortby. NULLS is
    -- unreserved (kwlist.h:315), so it is simultaneously a legal ColId and
    -- the lead-in to the nulls-order clause; Postgres separates the two
    -- readings with a two-token lexer lookahead (the NULLS_LA token,
    -- gram.y:864).
    it "sortby" $ do
      let render :: SortBy -> Text
          render = toText
      fmap render (parse @SortBy "a") `shouldBe` Right "a"
      fmap render (parse @SortBy "a asc") `shouldBe` Right "a ASC"
      fmap render (parse @SortBy "a desc nulls last") `shouldBe` Right "a DESC NULLS LAST"
      fmap render (parse @SortBy "a nulls first") `shouldBe` Right "a NULLS FIRST"
      fmap render (parse @SortBy "a using > nulls last") `shouldBe` Right "a USING > NULLS LAST"
      -- With the SortBy filter still blanket-excluding "nulls" from ColId,
      -- a bare column named "nulls" cannot currently be parsed as a
      -- SortBy target at all (not just as a nulls-order lead-in).
      -- Deviates from references/gram.y: Postgres's NULLS_LA lookahead
      -- (gram.y:864, 8596) only fires before FIRST/LAST, so a column
      -- literally named "nulls" is a legal bare sort key there.
      -- filteredColIdLike's blanket exclusion is coarser. Pre-existing;
      -- pinned here, not fixed — known follow-up.
      case parse @SortBy "nulls" of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected a parse failure for bare \"nulls\""
