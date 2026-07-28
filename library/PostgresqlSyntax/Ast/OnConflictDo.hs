module PostgresqlSyntax.Ast.OnConflictDo where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SetClauseList
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
-- @where_clause@ is a bare alias to 'PostgresqlSyntax.Ast.AExpr'.
data OnConflictDo
  = UpdateOnConflictDo SetClauseList (Maybe AExpr)
  | NothingOnConflictDo
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OnConflictDo where
  toTextBuilder = \case
    UpdateOnConflictDo a b -> "UPDATE SET " <> toTextBuilder a <> suffixMaybe whereClause b
    NothingOnConflictDo -> "NOTHING"
    where
      whereClause a = "WHERE " <> toTextBuilder a
  parser =
    asum
      [ NothingOnConflictDo <$ keyword "nothing",
        do
          keyword "update"
          Parser.space1
          Parser.endHead
          keyword "set"
          Parser.space1
          a <- parser
          b <- optional (Parser.space1 *> whereClause)
          return (UpdateOnConflictDo a b)
      ]
    where
      whereClause = keyword "where" *> Parser.space1 *> Parser.endHead *> parser

instance Qc.Arbitrary OnConflictDo where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ UpdateOnConflictDo <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.terminatingMaybe (Qc.downscale Qc.arbitrary),
        pure NothingOnConflictDo
      ]
