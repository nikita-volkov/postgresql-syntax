module PostgresqlSyntax.Ast.SetClause where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.SetTarget
import PostgresqlSyntax.Ast.SetTargetList
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- set_clause:
--   | set_target '=' a_expr
--   | '(' set_target_list ')' '=' a_expr
-- @
data SetClause
  = TargetSetClause SetTarget AExpr
  | TargetListSetClause SetTargetList AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SetClause where
  toTextBuilder settings = \case
    TargetSetClause a b -> toTextBuilder settings a <> " = " <> toTextBuilder settings b
    TargetListSetClause a b -> TextBuilders.renderInParens (toTextBuilder settings a) <> " = " <> toTextBuilder settings b
  parser settings =
    asum
      [ do
          a <- Parsers.inParens (parser settings)
          Parsers.space
          Parsers.char '='
          Parsers.space
          b <- parser settings
          return (TargetListSetClause a b),
        do
          a <- parser settings
          Parsers.space
          Parsers.char '='
          Parsers.space
          b <- parser settings
          return (TargetSetClause a b)
      ]

instance Qc.Arbitrary SetClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ TargetSetClause <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary,
        TargetListSetClause <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary
      ]
