module PostgresqlSyntax.Ast.Row where

import PostgresqlSyntax.Ast.ExplicitRow
import PostgresqlSyntax.Ast.ImplicitRow
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- row:
--   | ROW '(' expr_list ')'
--   | ROW '(' ')'
--   | '(' expr_list ',' a_expr ')'
-- @
data Row
  = ExplicitRowRow ExplicitRow
  | ImplicitRowRow ImplicitRow
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Row where
  toTextBuilder settings = \case
    ExplicitRowRow a -> toTextBuilder settings a
    ImplicitRowRow a -> toTextBuilder settings a
  parser settings = ExplicitRowRow <$> parser settings <|> ImplicitRowRow <$> parser settings

instance Qc.Arbitrary Row where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExplicitRowRow <$> Qc.arbitrary,
        ImplicitRowRow <$> Qc.arbitrary
      ]
