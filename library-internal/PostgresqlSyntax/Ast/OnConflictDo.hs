module PostgresqlSyntax.Ast.OnConflictDo where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.SetClauseList
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- opt_on_conflict:
--   | ON CONFLICT opt_conf_expr DO UPDATE SET set_clause_list where_clause
--   | ON CONFLICT opt_conf_expr DO NOTHING
--   | EMPTY
-- @
--
-- @where_clause@ is inlined here as a bare 'PostgresqlSyntax.Ast.AExpr'
-- rather than going through 'PostgresqlSyntax.Ast.WhereClause'.
data OnConflictDo
  = UpdateOnConflictDo SetClauseList (Maybe AExpr)
  | NothingOnConflictDo
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OnConflictDo where
  toTextBuilder settings = \case
    UpdateOnConflictDo a b -> "UPDATE SET " <> toTextBuilder settings a <> TextBuilders.suffixMaybe whereClause b
    NothingOnConflictDo -> "NOTHING"
    where
      whereClause a = "WHERE " <> toTextBuilder settings a
  parser settings =
    asum
      [ NothingOnConflictDo <$ Parsers.keyword "nothing",
        do
          Parsers.keyword "update"
          Parsers.space1
          Parser.endHead
          Parsers.keyword "set"
          Parsers.space1
          a <- parser settings
          b <- optional (Parsers.space1 *> whereClause)
          return (UpdateOnConflictDo a b)
      ]
    where
      whereClause = Parsers.keyword "where" *> Parsers.space1 *> Parser.endHead *> parser settings

instance Qc.Arbitrary OnConflictDo where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ UpdateOnConflictDo <$> Qc.arbitrary <*> Gens.terminatingMaybe (Gens.downscale Qc.arbitrary),
        pure NothingOnConflictDo
      ]
