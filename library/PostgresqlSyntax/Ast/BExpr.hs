module PostgresqlSyntax.Ast.BExpr where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.BExprIsOp
import PostgresqlSyntax.Ast.CExpr (CExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.QualOp
import PostgresqlSyntax.Ast.SymbolicExprBinOp
import PostgresqlSyntax.Ast.Typename
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- b_expr:
--   | c_expr
--   | b_expr TYPECAST Typename
--   | '+' b_expr
--   | '-' b_expr
--   | b_expr '+' b_expr
--   | b_expr '-' b_expr
--   | b_expr '*' b_expr
--   | b_expr '/' b_expr
--   | b_expr '%' b_expr
--   | b_expr '^' b_expr
--   | b_expr '<' b_expr
--   | b_expr '>' b_expr
--   | b_expr '=' b_expr
--   | b_expr LESS_EQUALS b_expr
--   | b_expr GREATER_EQUALS b_expr
--   | b_expr NOT_EQUALS b_expr
--   | b_expr qual_Op b_expr
--   | qual_Op b_expr
--   | b_expr qual_Op
--   | b_expr IS DISTINCT FROM b_expr
--   | b_expr IS NOT DISTINCT FROM b_expr
--   | b_expr IS OF '(' type_list ')'
--   | b_expr IS NOT OF '(' type_list ')'
--   | b_expr IS DOCUMENT_P
--   | b_expr IS NOT DOCUMENT_P
-- @
--
-- Unlike 'PostgresqlSyntax.Ast.AExpr', nothing customizes this parser's
-- @c_expr@\/identifier axis externally, so — despite also being a
-- recursion hub — it needs no @customizedParser@\/@filteredParser@ export.
data BExpr
  = CExprBExpr CExpr
  | TypecastBExpr BExpr Typename
  | PlusBExpr BExpr
  | MinusBExpr BExpr
  | SymbolicBinOpBExpr BExpr SymbolicExprBinOp BExpr
  | QualOpBExpr QualOp BExpr
  | IsOpBExpr BExpr Bool BExprIsOp
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst BExpr where
  toTextBuilder = \case
    CExprBExpr a -> toTextBuilder a
    TypecastBExpr a b -> toTextBuilder a <> " :: " <> toTextBuilder b
    PlusBExpr a -> "+ " <> toTextBuilder a
    MinusBExpr a -> "- " <> toTextBuilder a
    SymbolicBinOpBExpr a b c -> toTextBuilder a <> " " <> toTextBuilder b <> " " <> toTextBuilder c
    QualOpBExpr a b -> toTextBuilder a <> " " <> toTextBuilder b
    IsOpBExpr a b c -> toTextBuilder a <> " " <> renderBExprIsOp b c
    where
      renderBExprIsOp a =
        mappend (bool "IS " "IS NOT " a) . \case
          DistinctFromBExprIsOp b -> "DISTINCT FROM " <> toTextBuilder b
          OfBExprIsOp b -> "OF " <> renderInParens (toTextBuilder b)
          DocumentBExprIsOp -> "DOCUMENT"
  parser = suffixRec base suffix
    where
      bExpr = suffixRec base suffix
      base =
        asum
          [ qualOpExpr bExpr QualOpBExpr,
            PlusBExpr <$> plusedExpr bExpr,
            MinusBExpr <$> minusedExpr bExpr,
            CExprBExpr <$> parser
          ]
      suffix a =
        asum
          [ typecastExpr a TypecastBExpr,
            symbolicBinOpExpr a bExpr SymbolicBinOpBExpr,
            do
              space1
              keyword "is"
              space1
              endHead
              b <- trueIfPresent (keyword "not" *> space1)
              c <-
                asum
                  [ DistinctFromBExprIsOp <$> (keyphrase "distinct from" *> space1 *> endHead *> bExpr),
                    OfBExprIsOp <$> (keyword "of" *> space1 *> endHead *> inParens parser),
                    DocumentBExprIsOp <$ keyword "document"
                  ]
              return (IsOpBExpr a b c)
          ]

instance Arbitrary BExpr where
  arbitrary =
    sized $ \n ->
      if n <= 1
        then CExprBExpr <$> scale (`div` 2) arbitrary
        else
          oneof
            [ CExprBExpr <$> scale (`div` 2) arbitrary,
              TypecastBExpr <$> scale (`div` 2) arbitrary <*> arbitrary,
              PlusBExpr <$> scale (`div` 2) arbitrary,
              MinusBExpr <$> scale (`div` 2) arbitrary,
              SymbolicBinOpBExpr <$> scale (`div` 2) arbitrary <*> arbitrary <*> scale (`div` 2) arbitrary,
              QualOpBExpr <$> arbitrary <*> scale (`div` 2) arbitrary,
              IsOpBExpr <$> scale (`div` 2) arbitrary <*> arbitrary <*> scale (`div` 2) arbitrary
            ]
