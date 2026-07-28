module PostgresqlSyntax.Ast.ExplicitRow where

import PostgresqlSyntax.Ast.ExprList
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
      <> TextBuilders.renderInParens
        ( case a of
            EmptyExplicitRow -> mempty
            ExprListExplicitRow b -> toTextBuilder b
        )
  parser =
    Parsers.keyword "row"
      *> Parsers.space
      *> Parsers.inParens (maybe EmptyExplicitRow ExprListExplicitRow <$> optional parser)

instance Qc.Arbitrary ExplicitRow where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ pure EmptyExplicitRow,
        ExprListExplicitRow <$> Qc.scale (`div` 2) Qc.arbitrary
      ]
