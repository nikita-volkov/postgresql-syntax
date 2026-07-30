module PostgresqlSyntax.Ast.WhereOrCurrentClause where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- | WHERE a_expr
-- | WHERE CURRENT_P OF cursor_name
-- | /*EMPTY*/
-- @
--
-- @cursor_name@ is a bare alias to 'PostgresqlSyntax.Ast.Ident'.
data WhereOrCurrentClause
  = ExprWhereOrCurrentClause AExpr
  | CursorWhereOrCurrentClause Ident
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst WhereOrCurrentClause where
  toTextBuilder settings = \case
    ExprWhereOrCurrentClause a -> "WHERE " <> toTextBuilder settings a
    CursorWhereOrCurrentClause a -> "WHERE CURRENT OF " <> toTextBuilder settings a
  parser settings = do
    Parsers.keyword "where"
    Parsers.space1
    Parser.endHead
    asum
      [ do
          Parsers.keyword "current"
          Parsers.space1
          Parsers.keyword "of"
          Parsers.space1
          Parser.endHead
          a <- colId settings
          return (CursorWhereOrCurrentClause a),
        ExprWhereOrCurrentClause <$> parser settings
      ]

instance Qc.Arbitrary WhereOrCurrentClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprWhereOrCurrentClause <$> Gens.downscale Qc.arbitrary,
        CursorWhereOrCurrentClause <$> Qc.arbitrary
      ]
