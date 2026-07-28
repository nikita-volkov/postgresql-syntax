module PostgresqlSyntax.Ast.JoinQual where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- join_qual:
--   |  USING '(' name_list ')'
--   |  ON a_expr
-- @
data JoinQual
  = UsingJoinQual (NonEmpty Ident)
  | OnJoinQual AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst JoinQual where
  toTextBuilder = \case
    UsingJoinQual a -> "USING (" <> commaNonEmpty toTextBuilder a <> ")"
    OnJoinQual a -> "ON " <> toTextBuilder a
  parser =
    asum
      [ keyword "using" *> Parser.space1 *> inParens (Parser.sep1 commaSeparator colId) <&> UsingJoinQual,
        keyword "on" *> Parser.space1 *> parser <&> OnJoinQual
      ]

instance Qc.Arbitrary JoinQual where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ UsingJoinQual <$> Qc.nonEmptyUpTo 7 Qc.arbitrary,
        OnJoinQual <$> Qc.downscale Qc.arbitrary
      ]
