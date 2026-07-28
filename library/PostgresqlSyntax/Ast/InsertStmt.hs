module PostgresqlSyntax.Ast.InsertStmt where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.InsertRest
import PostgresqlSyntax.Ast.InsertTarget
import PostgresqlSyntax.Ast.OnConflict
import PostgresqlSyntax.Ast.TargetList
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
    TextBuilders.prefixMaybe toTextBuilder a
      <> "INSERT INTO "
      <> toTextBuilder b
      <> " "
      <> toTextBuilder c
      <> TextBuilders.suffixMaybe toTextBuilder d
      <> TextBuilders.suffixMaybe returningClause e
    where
      returningClause = mappend "RETURNING " . toTextBuilder
  parser = do
    a <- optional (Parser.wrapToHead parser <* Parsers.space1)
    Parsers.keyword "insert"
    Parsers.space1
    Parser.endHead
    Parsers.keyword "into"
    Parsers.space1
    b <- parser
    Parsers.space1
    c <- parser
    d <- optional (Parsers.space1 *> parser)
    e <- optional (Parsers.space1 *> returningClause)
    return (InsertStmt a b c d e)
    where
      returningClause = Parsers.keyword "returning" *> Parsers.space1 *> Parser.endHead *> parser

instance Qc.Arbitrary InsertStmt where
  shrink = Qc.genericShrink
  arbitrary =
    InsertStmt
      <$> Qc.terminatingMaybe (Qc.downscale Qc.arbitrary)
      <*> Qc.arbitrary
      <*> Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary
