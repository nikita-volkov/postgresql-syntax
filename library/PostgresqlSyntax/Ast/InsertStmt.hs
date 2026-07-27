module PostgresqlSyntax.Ast.InsertStmt where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.InsertRest
import PostgresqlSyntax.Ast.InsertTarget
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.OnConflict
import PostgresqlSyntax.Ast.TargetList
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- InsertStmt:
--   | opt_with_clause INSERT INTO insert_target insert_rest
--       opt_on_conflict returning_clause
-- @
--
-- @returning_clause@ is a bare alias to 'PostgresqlSyntax.Ast.TargetList'.
data InsertStmt = InsertStmt (Maybe WithClause) InsertTarget InsertRest (Maybe OnConflict) (Maybe TargetList)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InsertStmt where
  toTextBuilder (InsertStmt a b c d e) =
    prefixMaybe toTextBuilder a
      <> "INSERT INTO "
      <> toTextBuilder b
      <> " "
      <> toTextBuilder c
      <> suffixMaybe toTextBuilder d
      <> suffixMaybe returningClause e
    where
      returningClause = mappend "RETURNING " . toTextBuilder
  parser = do
    a <- optional (wrapToHead parser <* space1)
    keyword "insert"
    space1
    endHead
    keyword "into"
    space1
    b <- parser
    space1
    c <- parser
    d <- optional (space1 *> parser)
    e <- optional (space1 *> returningClause)
    return (InsertStmt a b c d e)
    where
      returningClause = keyword "returning" *> space1 *> endHead *> parser

instance Arbitrary InsertStmt where
  arbitrary =
    InsertStmt
      <$> scale (`div` 4) arbitrary
      <*> arbitrary
      <*> scale (`div` 2) arbitrary
      <*> scale (`div` 4) arbitrary
      <*> scale (`div` 4) arbitrary
