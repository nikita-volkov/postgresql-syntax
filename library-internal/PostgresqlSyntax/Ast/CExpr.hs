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
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import qualified PostgresqlSyntax.Extras.NonEmpty as NonEmpty
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import PostgresqlSyntax.Settings (Settings)
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
  toTextBuilder settings = \case
    ColumnrefCExpr a -> toTextBuilder settings a
    AexprConstCExpr a -> toTextBuilder settings a
    ParamCExpr a b -> "$" <> TextBuilder.intDec a <> foldMap (toTextBuilder settings) b
    InParensCExpr a b -> TextBuilders.renderInParens (toTextBuilder settings a) <> foldMap (toTextBuilder settings) b
    CaseCExpr a -> toTextBuilder settings a
    FuncCExpr a -> toTextBuilder settings a
    SelectWithParensCExpr a b -> toTextBuilder settings a <> foldMap (toTextBuilder settings) b
    ExistsCExpr a -> "EXISTS " <> toTextBuilder settings a
    ArrayCExpr a -> "ARRAY " <> either (toTextBuilder settings) (toTextBuilder settings) a
    ExplicitRowCExpr a -> toTextBuilder settings a
    ImplicitRowCExpr a -> toTextBuilder settings a
    GroupingCExpr a -> "GROUPING " <> TextBuilders.renderInParens (toTextBuilder settings a)
  parser settings = customizedParser settings (colId settings)

-- |
-- Parameterized over the @ColId@-like identifier parser used by the plain
-- 'ColumnrefCExpr' alternative — the one place 'PostgresqlSyntax.Ast.AExpr'
-- \'s @filteredParser@ needs to customize. Every other alternative here
-- (parenthesized expressions, @ARRAY@, @EXISTS@, function calls, ...) always
-- uses the ordinary, unfiltered parsers for its nested @a_expr@\/
-- @select_with_parens@\/etc, exactly as the pre-extraction
-- @customizedCExpr@\/@parenthesizedExprCExpr@ did — the filtering doesn't
-- propagate past this one level.
customizedParser :: Settings -> Parser Ident -> Parser CExpr
customizedParser settings colIdParser =
  asum
    [ ParamCExpr <$> (Parsers.char '$' *> Parsers.decimal <* Parser.endHead) <*> optional (Parsers.space *> parser settings),
      CaseCExpr <$> parser settings,
      ExplicitRowCExpr <$> parser settings,
      Parsers.inParensWithClause (Parsers.keyword "grouping") (GroupingCExpr . ExprList <$> Parsers.sep1 Parsers.commaSeparator (parser settings)),
      Parsers.keyword "exists" *> Parsers.space *> (ExistsCExpr <$> parser settings),
      do
        Parsers.keyword "array"
        Parsers.space
        asum
          [ ArrayCExpr . Right <$> parser settings,
            ArrayCExpr . Left <$> parser settings
          ],
      do
        a <- Parser.wrapToHead (parser settings)
        Parser.endHead
        b <- optional (Parsers.space *> parser settings)
        return (SelectWithParensCExpr a b),
      parenthesizedExprCExpr,
      AexprConstCExpr <$> Parser.wrapToHead (parser settings),
      FuncCExpr <$> parser settings,
      ColumnrefCExpr <$> customizedColumnref
    ]
  where
    customizedColumnref = do
      a <- Parser.wrapToHead colIdParser
      Parser.endHead
      b <- optional (Parsers.space *> parser settings)
      return (Columnref a b)

    -- See 'PostgresqlSyntax.Ast.AExpr'\'s doc on the sibling parser this
    -- replaces (@parenthesizedExprCExpr@\/implicit-row sharing trick) for
    -- why the single @a_expr@ parse is shared between the two endings.
    parenthesizedExprCExpr :: Parser CExpr
    parenthesizedExprCExpr = do
      Parsers.char '('
      Parsers.space
      a <- parser settings
      Parsers.space
      asum
        [ do
            Parsers.char ','
            Parser.endHead
            Parsers.space
            b <- Parsers.sep1 Parsers.commaSeparator (parser settings)
            Parsers.space
            Parsers.char ')'
            return $ ImplicitRowCExpr $ case NonEmpty.consAndUnsnoc a b of
              (c, d) -> ImplicitRow (ExprList c) d,
          do
            Parsers.char ')'
            Parser.endHead
            b <- optional (Parsers.space *> parser settings)
            return (InParensCExpr a b)
        ]

instance Qc.Arbitrary CExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then ColumnrefCExpr <$> Qc.arbitrary
        else
          Qc.oneof
            [ ColumnrefCExpr <$> Qc.arbitrary,
              AexprConstCExpr <$> Qc.arbitrary,
              ParamCExpr <$> Qc.choose (1, 19) <*> Qc.arbitrary,
              InParensCExpr <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary,
              CaseCExpr <$> Qc.arbitrary,
              FuncCExpr <$> Qc.arbitrary,
              SelectWithParensCExpr <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary,
              ExistsCExpr <$> Gens.downscale Qc.arbitrary,
              ArrayCExpr <$> Gens.downscale Qc.arbitrary,
              ExplicitRowCExpr <$> Qc.arbitrary,
              ImplicitRowCExpr <$> Qc.arbitrary,
              GroupingCExpr <$> Qc.arbitrary
            ]
