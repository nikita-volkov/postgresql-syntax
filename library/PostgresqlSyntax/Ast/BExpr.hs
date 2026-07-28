module PostgresqlSyntax.Ast.BExpr where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.BExprIsOp
import PostgresqlSyntax.Ast.CExpr (CExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.QualOp
import PostgresqlSyntax.Ast.SymbolicExprBinOp
import PostgresqlSyntax.Ast.Typename
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
    TypecastBExpr a b -> renderOperand a <> " :: " <> toTextBuilder b
    PlusBExpr a -> "+ " <> toTextBuilder a
    MinusBExpr a -> "- " <> toTextBuilder a
    SymbolicBinOpBExpr a b c -> renderOperand a <> " " <> toTextBuilder b <> " " <> toTextBuilder c
    QualOpBExpr a b -> toTextBuilder a <> " " <> toTextBuilder b
    IsOpBExpr a b c -> renderOperand a <> " " <> renderBExprIsOp b c
    where
      -- See 'PostgresqlSyntax.Ast.AExpr'\'s @renderOperand@ for the
      -- rationale — same left\/accumulator-position hazard, mirrored here
      -- for 'BExpr'\'s own (smaller) suffix grammar. Unlike 'AExpr', there's
      -- no @'(' b_expr ')'@ production to fall back on, so parenthesizing
      -- reinterprets the operand as an @a_expr@ via
      -- 'PostgresqlSyntax.Ast.CExpr'\'s @'(' a_expr ')'@ instead — still
      -- valid, semantically-equivalent SQL, just not the same 'BExpr' shape
      -- on reparse (only relevant to hand-constructed values; the
      -- 'Qc.Arbitrary' instance below never generates an operand needing
      -- this fallback).
      renderOperand a
        | isBoundedBExprOperand a = toTextBuilder a
        | otherwise = renderInParens (toTextBuilder a)
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

-- |
-- Whether the given 'BExpr' is safe to place in the left\/accumulator
-- position of a suffix production without parenthesizing it — see
-- 'IsAst' 'BExpr'\'s @renderOperand@. Mirrors
-- 'PostgresqlSyntax.Ast.AExpr.isBoundedAExprOperand'.
isBoundedBExprOperand :: BExpr -> Bool
isBoundedBExprOperand = \case
  PlusBExpr {} -> False
  MinusBExpr {} -> False
  QualOpBExpr {} -> False
  SymbolicBinOpBExpr {} -> False
  IsOpBExpr _ _ c -> case c of
    DistinctFromBExprIsOp {} -> False
    _ -> True
  _ -> True

-- |
-- A generator for the left\/accumulator position of a suffix production
-- (see 'isBoundedBExprOperand'). Unlike
-- 'PostgresqlSyntax.Ast.AExpr.safeAExprOperand', 'BExpr' has no
-- parenthesizing escape hatch of its own (see @renderOperand@ above), so an
-- unbounded draw is simply replaced by an always-bounded 'CExprBExpr'
-- instead of wrapped.
safeBExprOperand :: Qc.Gen BExpr -> Qc.Gen BExpr
safeBExprOperand gen = do
  a <- gen
  if isBoundedBExprOperand a
    then pure a
    else CExprBExpr <$> Qc.arbitrary

instance Qc.Arbitrary BExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then CExprBExpr <$> Qc.arbitrary
        else
          Qc.oneof
            [ CExprBExpr <$> Qc.arbitrary,
              TypecastBExpr <$> safeBExprOperand (Qc.downscale Qc.arbitrary) <*> Qc.arbitrary,
              PlusBExpr <$> Qc.downscale Qc.arbitrary,
              MinusBExpr <$> Qc.downscale Qc.arbitrary,
              SymbolicBinOpBExpr <$> safeBExprOperand (Qc.downscale Qc.arbitrary) <*> Qc.arbitrary <*> Qc.downscale Qc.arbitrary,
              QualOpBExpr <$> Qc.arbitrary <*> Qc.downscale Qc.arbitrary,
              IsOpBExpr <$> safeBExprOperand (Qc.downscale Qc.arbitrary) <*> Qc.arbitrary <*> Qc.downscale Qc.arbitrary
            ]
