module PostgresqlSyntax.Ast.AExprReversableOp where

import HeadedMegaparsec
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import {-# SOURCE #-} PostgresqlSyntax.Ast.BExpr (BExpr)
import PostgresqlSyntax.Ast.InExpr
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TypeList
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- The part of the following productions that follows @a_expr [NOT]@ /
-- @b_expr [NOT]@ — the leading @IS@\/@NOT@ toggle itself is external to this
-- type (it lives alongside it, e.g. in @ReversableOpAExpr AExpr Bool
-- AExprReversableOp@), mirroring how 'PostgresqlSyntax.Ast.VerbalExprBinOp'
-- keeps @NOT_LA@ external. Only the @IS@\/@BETWEEN@\/@IN@ keyword that's
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
  toTextBuilder = \case
    NullAExprReversableOp -> "IS NULL"
    TrueAExprReversableOp -> "IS TRUE"
    FalseAExprReversableOp -> "IS FALSE"
    UnknownAExprReversableOp -> "IS UNKNOWN"
    DistinctFromAExprReversableOp b -> "IS DISTINCT FROM " <> toTextBuilder b
    OfAExprReversableOp b -> "IS OF " <> renderInParens (toTextBuilder b)
    BetweenAExprReversableOp b c d -> bool "BETWEEN " "BETWEEN ASYMMETRIC " b <> toTextBuilder c <> " AND " <> toTextBuilder d
    BetweenSymmetricAExprReversableOp b c -> "BETWEEN SYMMETRIC " <> toTextBuilder b <> " AND " <> toTextBuilder c
    InAExprReversableOp b -> "IN " <> toTextBuilder b
    DocumentAExprReversableOp -> "IS DOCUMENT"
  parser =
    asum
      [ keyword "is"
          *> space1
          *> endHead
          *> asum
            [ NullAExprReversableOp <$ keyword "null",
              TrueAExprReversableOp <$ keyword "true",
              FalseAExprReversableOp <$ keyword "false",
              UnknownAExprReversableOp <$ keyword "unknown",
              DistinctFromAExprReversableOp <$> (keyword "distinct" *> space1 *> keyword "from" *> space1 *> endHead *> parser),
              OfAExprReversableOp <$> (keyword "of" *> space1 *> endHead *> inParens parser),
              DocumentAExprReversableOp <$ keyword "document"
            ],
        do
          keyword "between"
          space1
          endHead
          c <-
            asum
              [ BetweenSymmetricAExprReversableOp <$ (keyword "symmetric" *> space1),
                BetweenAExprReversableOp True <$ (keyword "asymmetric" *> space1),
                pure (BetweenAExprReversableOp False)
              ]
          d <- parser
          space1
          keyword "and"
          space1
          e <- parser
          return (c d e),
        InAExprReversableOp <$> (keyword "in" *> space *> parser)
      ]

instance Arbitrary AExprReversableOp where
  arbitrary =
    sized $ \n ->
      if n <= 1
        then oneof [pure NullAExprReversableOp, pure TrueAExprReversableOp, pure FalseAExprReversableOp, pure UnknownAExprReversableOp, pure DocumentAExprReversableOp]
        else
          oneof
            [ pure NullAExprReversableOp,
              pure TrueAExprReversableOp,
              pure FalseAExprReversableOp,
              pure UnknownAExprReversableOp,
              DistinctFromAExprReversableOp <$> scale (`div` 2) arbitrary,
              OfAExprReversableOp <$> scale (`div` 2) arbitrary,
              BetweenAExprReversableOp <$> arbitrary <*> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
              BetweenSymmetricAExprReversableOp <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
              InAExprReversableOp <$> scale (`div` 2) arbitrary,
              pure DocumentAExprReversableOp
            ]
