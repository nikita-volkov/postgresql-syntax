module PostgresqlSyntax.Ast.ExplicitRow where

import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- explicit_row:
--   | ROW '(' expr_list ')'
--   | ROW '(' ')'
-- @
data ExplicitRow
  = EmptyExplicitRow
  | ExprListExplicitRow ExprList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ExplicitRow where
  toTextBuilder a =
    "ROW "
      <> renderInParens
        ( case a of
            EmptyExplicitRow -> mempty
            ExprListExplicitRow b -> toTextBuilder b
        )
  parser =
    keyword "row"
      *> space
      *> inParens (maybe EmptyExplicitRow ExprListExplicitRow <$> optional parser)

instance Arbitrary ExplicitRow where
  arbitrary =
    oneof
      [ pure EmptyExplicitRow,
        ExprListExplicitRow <$> scale (`div` 2) arbitrary
      ]
