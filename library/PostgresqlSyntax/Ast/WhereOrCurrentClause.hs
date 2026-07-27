module PostgresqlSyntax.Ast.WhereOrCurrentClause where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

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
    space1
    endHead
    asum
      [ do
          keyword "current"
          space1
          keyword "of"
          space1
          endHead
          a <- colId
          return (CursorWhereOrCurrentClause a),
        ExprWhereOrCurrentClause <$> parser
      ]

instance Arbitrary WhereOrCurrentClause where
  arbitrary =
    oneof
      [ ExprWhereOrCurrentClause <$> scale (`div` 2) arbitrary,
        CursorWhereOrCurrentClause <$> arbitrary
      ]
