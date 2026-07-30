module PostgresqlSyntax.Ast.InsertStmt where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.InsertRest
import PostgresqlSyntax.Ast.InsertTarget
import PostgresqlSyntax.Ast.OnConflict
import PostgresqlSyntax.Ast.ReturningClause
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- InsertStmt:
--   | opt_with_clause INSERT INTO insert_target insert_rest
--       opt_on_conflict returning_clause
-- @
--
data InsertStmt = InsertStmt (Maybe WithClause) InsertTarget InsertRest (Maybe OnConflict) (Maybe ReturningClause)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InsertStmt where
  toTextBuilder settings (InsertStmt a b c d e) =
    TextBuilders.prefixMaybe (toTextBuilder settings) a
      <> "INSERT INTO "
      <> toTextBuilder settings b
      <> " "
      <> toTextBuilder settings c
      <> TextBuilders.suffixMaybe (toTextBuilder settings) d
      <> TextBuilders.suffixMaybe (toTextBuilder settings) e
  parser settings = do
    a <- optional (Parser.wrapToHead (parser settings) <* Parsers.space1)
    Parsers.keyword "insert"
    Parsers.space1
    Parser.endHead
    Parsers.keyword "into"
    Parsers.space1
    b <- parser settings
    Parsers.space1
    c <- parser settings
    d <- optional (Parsers.space1 *> parser settings)
    e <- optional (Parsers.space1 *> parser settings)
    return (InsertStmt a b c d e)

instance Qc.Arbitrary InsertStmt where
  shrink = Qc.genericShrink
  arbitrary =
    InsertStmt
      <$> Gens.terminatingMaybe (Gens.downscale Qc.arbitrary)
      <*> Qc.arbitrary
      <*> Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
