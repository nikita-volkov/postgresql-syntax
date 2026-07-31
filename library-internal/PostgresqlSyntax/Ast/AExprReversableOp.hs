module PostgresqlSyntax.Ast.AExprReversableOp where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import {-# SOURCE #-} PostgresqlSyntax.Ast.BExpr (BExpr)
import PostgresqlSyntax.Ast.InExpr
import PostgresqlSyntax.Ast.TypeList
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- The part of the following productions that follows @a_expr [NOT]@ /
-- @b_expr [NOT]@ — the leading @IS@\/@NOT@ toggle itself is external to this
-- type (it lives alongside it, e.g. in @ReversableOpAExpr AExpr Bool
-- AExprReversableOp@), mirroring how 'PostgresqlSyntax.Ast.VerbalExprBinOp'
-- keeps @NOT_LA@ external. Only the @IS@\/@BETWEEN@\/@IN@ Parsers.keyword that's
-- intrinsic to each specific alternative (as opposed to the shared negation)
-- is captured here.
--
-- ==== References
-- @
--   | a_expr IS NULL_P
--   | a_expr IS TRUE_P
--   | a_expr IS FALSE_P
--   | a_expr IS UNKNOWN
--   | a_expr IS DISTINCT FROM a_expr
--   | a_expr IS OF '(' type_list ')'
--   | a_expr BETWEEN opt_asymmetric b_expr AND a_expr
--   | a_expr BETWEEN SYMMETRIC b_expr AND a_expr
--   | a_expr IN_P in_expr
--   | a_expr IS DOCUMENT_P
-- @
data AExprReversableOp
  = NullAExprReversableOp
  | TrueAExprReversableOp
  | FalseAExprReversableOp
  | UnknownAExprReversableOp
  | DistinctFromAExprReversableOp AExpr
  | OfAExprReversableOp TypeList
  | BetweenAExprReversableOp Bool BExpr AExpr
  | BetweenSymmetricAExprReversableOp BExpr AExpr
  | InAExprReversableOp InExpr
  | DocumentAExprReversableOp
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst AExprReversableOp where
  toTextBuilder settings = \case
    NullAExprReversableOp -> "IS NULL"
    TrueAExprReversableOp -> "IS TRUE"
    FalseAExprReversableOp -> "IS FALSE"
    UnknownAExprReversableOp -> "IS UNKNOWN"
    DistinctFromAExprReversableOp b -> "IS DISTINCT FROM " <> toTextBuilder settings b
    OfAExprReversableOp b -> "IS OF " <> TextBuilders.renderInParens (toTextBuilder settings b)
    BetweenAExprReversableOp b c d -> bool "BETWEEN " "BETWEEN ASYMMETRIC " b <> toTextBuilder settings c <> " AND " <> toTextBuilder settings d
    BetweenSymmetricAExprReversableOp b c -> "BETWEEN SYMMETRIC " <> toTextBuilder settings b <> " AND " <> toTextBuilder settings c
    InAExprReversableOp b -> "IN " <> toTextBuilder settings b
    DocumentAExprReversableOp -> "IS DOCUMENT"
  parser settings =
    asum
      [ Parsers.keyword "is"
          *> Parsers.space1
          *> Parser.endHead
          *> asum
            [ NullAExprReversableOp <$ Parsers.keyword "null",
              TrueAExprReversableOp <$ Parsers.keyword "true",
              FalseAExprReversableOp <$ Parsers.keyword "false",
              UnknownAExprReversableOp <$ Parsers.keyword "unknown",
              DistinctFromAExprReversableOp <$> (Parsers.keyword "distinct" *> Parsers.space1 *> Parsers.keyword "from" *> Parsers.space1 *> Parser.endHead *> parser settings),
              OfAExprReversableOp <$> (Parsers.keyword "of" *> Parsers.space1 *> Parser.endHead *> Parsers.inParens (parser settings)),
              DocumentAExprReversableOp <$ Parsers.keyword "document"
            ],
        do
          Parsers.keyword "between"
          Parsers.space1
          Parser.endHead
          c <-
            asum
              [ BetweenSymmetricAExprReversableOp <$ (Parsers.keyword "symmetric" *> Parsers.space1),
                BetweenAExprReversableOp True <$ (Parsers.keyword "asymmetric" *> Parsers.space1),
                pure (BetweenAExprReversableOp False)
              ]
          d <- parser settings
          Parsers.space1
          Parsers.keyword "and"
          Parsers.space1
          e <- parser settings
          return (c d e),
        InAExprReversableOp <$> (Parsers.keyword "in" *> Parsers.space *> parser settings)
      ]

instance Qc.Arbitrary AExprReversableOp where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then Qc.oneof [pure NullAExprReversableOp, pure TrueAExprReversableOp, pure FalseAExprReversableOp, pure UnknownAExprReversableOp, pure DocumentAExprReversableOp]
        else
          Qc.oneof
            [ pure NullAExprReversableOp,
              pure TrueAExprReversableOp,
              pure FalseAExprReversableOp,
              pure UnknownAExprReversableOp,
              DistinctFromAExprReversableOp <$> Gens.downscale Qc.arbitrary,
              OfAExprReversableOp <$> Gens.downscale Qc.arbitrary,
              BetweenAExprReversableOp <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary <*> Gens.downscale Qc.arbitrary,
              BetweenSymmetricAExprReversableOp <$> Gens.downscale Qc.arbitrary <*> Gens.downscale Qc.arbitrary,
              InAExprReversableOp <$> Gens.downscale Qc.arbitrary,
              pure DocumentAExprReversableOp
            ]
