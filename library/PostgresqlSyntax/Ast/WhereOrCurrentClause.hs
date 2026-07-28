module PostgresqlSyntax.Ast.WhereOrCurrentClause where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder = \case
    ExprWhereOrCurrentClause a -> "WHERE " <> toTextBuilder a
    CursorWhereOrCurrentClause a -> "WHERE CURRENT OF " <> toTextBuilder a
  parser = do
    keyword "where"
    Parser.space1
    Parser.endHead
    asum
      [ do
          keyword "current"
          Parser.space1
          keyword "of"
          Parser.space1
          Parser.endHead
          a <- colId
          return (CursorWhereOrCurrentClause a),
        ExprWhereOrCurrentClause <$> parser
      ]

instance Qc.Arbitrary WhereOrCurrentClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprWhereOrCurrentClause <$> Qc.scale (`div` 2) Qc.arbitrary,
        CursorWhereOrCurrentClause <$> Qc.arbitrary
      ]
