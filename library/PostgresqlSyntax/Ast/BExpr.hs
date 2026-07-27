module PostgresqlSyntax.Ast.BExpr where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.BExprIsOp
import PostgresqlSyntax.Ast.CExpr (CExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.QualOp
import PostgresqlSyntax.Ast.SymbolicExprBinOp
import PostgresqlSyntax.Ast.Typename
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
              Parser.space1
              keyword "is"
              Parser.space1
              Parser.endHead
              b <- trueIfPresent (keyword "not" *> Parser.space1)
              c <-
                asum
                  [ DistinctFromBExprIsOp <$> (keyphrase "distinct from" *> Parser.space1 *> Parser.endHead *> bExpr),
                    OfBExprIsOp <$> (keyword "of" *> Parser.space1 *> Parser.endHead *> inParens parser),
                    DocumentBExprIsOp <$ keyword "document"
                  ]
              return (IsOpBExpr a b c)
          ]

instance Qc.Arbitrary BExpr where
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then CExprBExpr <$> Qc.scale (`div` 2) Qc.arbitrary
        else
          Qc.oneof
            [ CExprBExpr <$> Qc.scale (`div` 2) Qc.arbitrary,
              TypecastBExpr <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary,
              PlusBExpr <$> Qc.scale (`div` 2) Qc.arbitrary,
              MinusBExpr <$> Qc.scale (`div` 2) Qc.arbitrary,
              SymbolicBinOpBExpr <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
              QualOpBExpr <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
              IsOpBExpr <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary
            ]
