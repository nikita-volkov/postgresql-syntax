module PostgresqlSyntax.Ast.SelectLimitValue where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- select_limit_value:
--   | a_expr
--   | ALL
-- @
data SelectLimitValue
  = ExprSelectLimitValue AExpr
  | AllSelectLimitValue
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SelectLimitValue where
  toTextBuilder settings = \case
    ExprSelectLimitValue a -> toTextBuilder settings a
    AllSelectLimitValue -> "ALL"
  parser settings =
    AllSelectLimitValue
      <$ Parsers.keyword "all"
        <|> ExprSelectLimitValue
      <$> parser settings

instance Qc.Arbitrary SelectLimitValue where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprSelectLimitValue <$> Gens.downscale Qc.arbitrary,
        pure AllSelectLimitValue
      ]
