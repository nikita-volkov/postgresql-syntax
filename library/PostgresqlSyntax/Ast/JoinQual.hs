module PostgresqlSyntax.Ast.JoinQual where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
    UsingJoinQual a -> "USING (" <> TextBuilders.commaNonEmpty toTextBuilder a <> ")"
    OnJoinQual a -> "ON " <> toTextBuilder a
  parser =
    asum
      [ Parsers.keyword "using" *> Parsers.space1 *> Parsers.inParens (Parsers.sep1 Parsers.commaSeparator colId) <&> UsingJoinQual,
        Parsers.keyword "on" *> Parsers.space1 *> parser <&> OnJoinQual
      ]

instance Qc.Arbitrary JoinQual where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ UsingJoinQual <$> Gens.nonEmptyUpTo 7 Qc.arbitrary,
        OnJoinQual <$> Gens.downscale Qc.arbitrary
      ]
