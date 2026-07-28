module PostgresqlSyntax.Ast.ExplicitRow where

import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
      <> renderInParens
        ( case a of
            EmptyExplicitRow -> mempty
            ExprListExplicitRow b -> toTextBuilder b
        )
  parser =
    keyword "row"
      *> Parser.space
      *> inParens (maybe EmptyExplicitRow ExprListExplicitRow <$> optional parser)

instance Qc.Arbitrary ExplicitRow where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ pure EmptyExplicitRow,
        ExprListExplicitRow <$> Qc.arbitrary
      ]
