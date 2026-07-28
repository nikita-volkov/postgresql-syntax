module PostgresqlSyntax.Ast.SelectLimitValue where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder = \case
    ExprSelectLimitValue a -> toTextBuilder a
    AllSelectLimitValue -> "ALL"
  parser =
    AllSelectLimitValue
      <$ Parsers.keyword "all"
        <|> ExprSelectLimitValue
      <$> parser

instance Qc.Arbitrary SelectLimitValue where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprSelectLimitValue <$> Gens.downscale Qc.arbitrary,
        pure AllSelectLimitValue
      ]
