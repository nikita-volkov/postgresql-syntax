module PostgresqlSyntax.Ast.InsertStmt where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.InsertRest
import PostgresqlSyntax.Ast.InsertTarget
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.OnConflict
import PostgresqlSyntax.Ast.TargetList
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
    a <- optional (Parser.wrapToHead parser <* Parser.space1)
    keyword "insert"
    Parser.space1
    Parser.endHead
    keyword "into"
    Parser.space1
    b <- parser
    Parser.space1
    c <- parser
    d <- optional (Parser.space1 *> parser)
    e <- optional (Parser.space1 *> returningClause)
    return (InsertStmt a b c d e)
    where
      returningClause = keyword "returning" *> Parser.space1 *> Parser.endHead *> parser

instance Qc.Arbitrary InsertStmt where
  shrink = Qc.genericShrink
  arbitrary =
    InsertStmt
      <$> Qc.scale (`div` 4) Qc.arbitrary
      <*> Qc.arbitrary
      <*> Qc.scale (`div` 2) Qc.arbitrary
      <*> Qc.scale (`div` 4) Qc.arbitrary
      <*> Qc.scale (`div` 4) Qc.arbitrary
