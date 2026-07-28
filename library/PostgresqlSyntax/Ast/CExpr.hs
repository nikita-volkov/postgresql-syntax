module PostgresqlSyntax.Ast.CExpr
  ( CExpr (..),
    customizedParser,
  )
where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.AexprConst
import PostgresqlSyntax.Ast.ArrayExpr
import PostgresqlSyntax.Ast.CaseExpr
import PostgresqlSyntax.Ast.Columnref
import PostgresqlSyntax.Ast.ExplicitRow
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.FuncExpr
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.ImplicitRow
import PostgresqlSyntax.Ast.Indirection
import PostgresqlSyntax.Ast.Internal
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.NonEmpty as NonEmpty
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- c_expr:
--   | columnref
--   | AexprConst
--   | PARAM opt_indirection
--   | '(' a_expr ')' opt_indirection
--   | case_expr
--   | func_expr
--   | select_with_parens
--   | select_with_parens indirection
--   | EXISTS select_with_parens
--   | ARRAY select_with_parens
--   | ARRAY array_expr
--   | explicit_row
--   | implicit_row
--   | GROUPING '(' expr_list ')'
-- @
data CExpr
  = ColumnrefCExpr Columnref
  | AexprConstCExpr AexprConst
  | ParamCExpr Int (Maybe Indirection)
  | InParensCExpr AExpr (Maybe Indirection)
  | CaseCExpr CaseExpr
  | FuncCExpr FuncExpr
  | SelectWithParensCExpr SelectWithParens (Maybe Indirection)
  | ExistsCExpr SelectWithParens
  | ArrayCExpr (Either SelectWithParens ArrayExpr)
  | ExplicitRowCExpr ExplicitRow
  | ImplicitRowCExpr ImplicitRow
  | GroupingCExpr ExprList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst CExpr where
  toTextBuilder = \case
    ColumnrefCExpr a -> toTextBuilder a
    AexprConstCExpr a -> toTextBuilder a
    ParamCExpr a b -> "$" <> TextBuilder.intDec a <> foldMap toTextBuilder b
    InParensCExpr a b -> renderInParens (toTextBuilder a) <> foldMap toTextBuilder b
    CaseCExpr a -> toTextBuilder a
    FuncCExpr a -> toTextBuilder a
    SelectWithParensCExpr a b -> toTextBuilder a <> foldMap toTextBuilder b
    ExistsCExpr a -> "EXISTS " <> toTextBuilder a
    ArrayCExpr a -> "ARRAY " <> either toTextBuilder toTextBuilder a
    ExplicitRowCExpr a -> toTextBuilder a
    ImplicitRowCExpr a -> toTextBuilder a
    GroupingCExpr a -> "GROUPING " <> renderInParens (toTextBuilder a)
  parser = customizedParser colId

-- |
-- Parameterized over the @ColId@-like identifier parser used by the plain
-- 'ColumnrefCExpr' alternative — the one place 'PostgresqlSyntax.Ast.AExpr'
-- \'s @filteredParser@ needs to customize. Every other alternative here
-- (parenthesized expressions, @ARRAY@, @EXISTS@, function calls, ...) always
-- uses the ordinary, unfiltered parsers for its nested @a_expr@\/
-- @select_with_parens@\/etc, exactly as the pre-extraction
-- @customizedCExpr@\/@parenthesizedExprCExpr@ did — the filtering doesn't
-- propagate past this one level.
customizedParser :: Parser Ident -> Parser CExpr
customizedParser colIdParser =
  asum
    [ ParamCExpr <$> (Parser.char '$' *> Parser.decimal <* Parser.endHead) <*> optional (Parser.space *> parser),
      CaseCExpr <$> parser,
      ExplicitRowCExpr <$> parser,
      inParensWithClause (keyword "grouping") (GroupingCExpr . ExprList <$> Parser.sep1 commaSeparator parser),
      keyword "exists" *> Parser.space *> (ExistsCExpr <$> parser),
      do
        keyword "array"
        Parser.space
        asum
          [ ArrayCExpr . Right <$> parser,
            ArrayCExpr . Left <$> parser
          ],
      do
        a <- Parser.wrapToHead parser
        Parser.endHead
        b <- optional (Parser.space *> parser)
        return (SelectWithParensCExpr a b),
      parenthesizedExprCExpr,
      AexprConstCExpr <$> Parser.wrapToHead parser,
      FuncCExpr <$> parser,
      ColumnrefCExpr <$> customizedColumnref
    ]
  where
    customizedColumnref = do
      a <- Parser.wrapToHead colIdParser
      Parser.endHead
      b <- optional (Parser.space *> parser)
      return (Columnref a b)
    -- \|
    -- See 'PostgresqlSyntax.Ast.AExpr'\'s doc on the sibling parser this
    -- replaces (@parenthesizedExprCExpr@\/implicit-row sharing trick) for
    -- why the single @a_expr@ parse is shared between the two endings.
    parenthesizedExprCExpr :: Parser CExpr
    parenthesizedExprCExpr = do
      Parser.char '('
      Parser.space
      a <- parser
      Parser.space
      asum
        [ do
            Parser.char ','
            Parser.endHead
            Parser.space
            b <- Parser.sep1 commaSeparator parser
            Parser.space
            Parser.char ')'
            return $ ImplicitRowCExpr $ case NonEmpty.consAndUnsnoc a b of
              (c, d) -> ImplicitRow (ExprList c) d,
          do
            Parser.char ')'
            Parser.endHead
            b <- optional (Parser.space *> parser)
            return (InParensCExpr a b)
        ]

instance Qc.Arbitrary CExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then ColumnrefCExpr <$> Qc.arbitrary
        else
          Qc.scale (`div` 2) $
            Qc.oneof
              [ ColumnrefCExpr <$> Qc.arbitrary,
                AexprConstCExpr <$> Qc.arbitrary,
                ParamCExpr <$> Qc.choose (1, 19) <*> Qc.arbitrary,
                InParensCExpr <$> Qc.arbitrary <*> Qc.arbitrary,
                CaseCExpr <$> Qc.arbitrary,
                FuncCExpr <$> Qc.arbitrary,
                SelectWithParensCExpr <$> Qc.arbitrary <*> Qc.arbitrary,
                ExistsCExpr <$> Qc.arbitrary,
                ArrayCExpr <$> Qc.arbitrary,
                ExplicitRowCExpr <$> Qc.arbitrary,
                ImplicitRowCExpr <$> Qc.arbitrary,
                GroupingCExpr <$> Qc.arbitrary
              ]
