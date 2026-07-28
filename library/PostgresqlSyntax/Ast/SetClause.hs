module PostgresqlSyntax.Ast.SetClause where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SetTarget
import PostgresqlSyntax.Ast.SetTargetList
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder = \case
    TargetSetClause a b -> toTextBuilder a <> " = " <> toTextBuilder b
    TargetListSetClause a b -> renderInParens (toTextBuilder a) <> " = " <> toTextBuilder b
  parser =
    asum
      [ do
          a <- inParens parser
          Parser.space
          Parser.char '='
          Parser.space
          b <- parser
          return (TargetListSetClause a b),
        do
          a <- parser
          Parser.space
          Parser.char '='
          Parser.space
          b <- parser
          return (TargetSetClause a b)
      ]

instance Qc.Arbitrary SetClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ TargetSetClause <$> Qc.arbitrary <*> Qc.downscale Qc.arbitrary,
        TargetListSetClause <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.downscale Qc.arbitrary
      ]
