module Ast.PreparableStmtSpec (spec) where

import qualified Data.Text as Text
import Helpers.Specs
import PostgresqlSyntax.Ast.PreparableStmt
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @PreparableStmt
  itSatisfiesArbitrary @PreparableStmt
  describe "Parsers" $ do
    itParses @PreparableStmt
      "select i :: int8 from auth.user as u\n\
      \inner join edgenode.usere_provider as p\n\
      \on u.id = p.user_id\n\
      \inner join edgenode.provider_branch as b\n\
      \on b.provider_fk = p.provider_id"
    itParses @PreparableStmt "select * from items for update limit 1"
    itParses @PreparableStmt "select * from items limit 1 for update"
    itParses @PreparableStmt "select * from items for share limit 10"
    itParses @PreparableStmt "select * from items for no key update limit 1"
    itParses @PreparableStmt "select * from items for key share limit 1"
    itParses @PreparableStmt "select * from items for update of items nowait limit 1"
    itParses @PreparableStmt "select * from items for update skip locked limit 1"
    itParses @PreparableStmt "select * from items order by id for update limit 1"
    itParses @PreparableStmt "select * from items for update offset 5 limit 10"
  describe "Nesting depth" $ do
    itParsesWithin @PreparableStmt 5 ("select " <> Text.replicate 50 "(" <> "a + b" <> Text.replicate 50 ")")
  describe "Error reporting" $ do
    itReportsError @PreparableStmt
      "select i :: int8 fom auth.user as u\n\
      \inner join edgenode.usere_provider as p\n\
      \on u.id = p.user_id\n\
      \inner join edgenode.provider_branch as b\n\
      \on b.provider_fk = p.provider_id"
      "(21,\"offset=21:\\nunexpected 'a'\\nexpecting end of input or white space\\n\")"
    itReportsError @PreparableStmt
      "select i :: int8 from auth.user as u\n\
      \WHERE u.id IS NO NULL && TRUE"
      "(51,\"offset=51:\\nexpecting white space\\n\")"
