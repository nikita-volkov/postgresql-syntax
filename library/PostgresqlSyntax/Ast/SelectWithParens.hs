module PostgresqlSyntax.Ast.SelectWithParens where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectNoParens (SelectNoParens, afterSelectWithParensClause, unparenthesizedSelectNoParens)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (discard, frequency, scale)

-- |
-- ==== References
-- @
-- select_with_parens:
--   |  '(' select_no_parens ')'
--   |  '(' select_with_parens ')'
-- @
data SelectWithParens
  = NoParensSelectWithParens SelectNoParens
  | WithParensSelectWithParens SelectWithParens
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SelectWithParens where
  toTextBuilder =
    renderInParens . \case
      NoParensSelectWithParens a -> toTextBuilder a
      WithParensSelectWithParens a -> toTextBuilder a

  -- \|
  -- @gram.y@ gives two productions, @'(' select_with_parens ')'@ and
  -- @'(' select_no_parens ')'@, and they overlap: a @select_no_parens@ may
  -- itself be nothing but a @select_clause@, and a @select_clause@ may
  -- itself be a @select_with_parens@. Transcribed literally as two
  -- alternatives, that overlap makes every nested paren group get parsed
  -- twice — once down the @select_with_parens@ branch and once down the
  -- @select_no_parens@ branch — so the cost doubles with each level of
  -- nesting. So the shared prefix is parsed once and classified afterwards.
  --
  -- ==== Canonical shape
  --
  -- Because the productions overlap, @((select 1))@ has two
  -- representations: @WithParensSelectWithParens@ wrapping the inner
  -- parenthesised select, or @NoParensSelectWithParens@ of a
  -- @SelectNoParens@ whose clause is that same inner parenthesised select.
  -- Both render back to the same text. __The first is canonical.__
  parser = inParens selectWithParensBody
    where
      selectWithParensBody =
        asum
          [ do
              a <- wrapToHead parser
              either WithParensSelectWithParens NoParensSelectWithParens <$> afterSelectWithParensClause a,
            NoParensSelectWithParens <$> unparenthesizedSelectNoParens
          ]

instance Arbitrary SelectWithParens where
  arbitrary =
    sized $ \size ->
      if size <= 1
        then discard
        else
          frequency
            [ (95, NoParensSelectWithParens <$> scale (`div` 2) arbitrary),
              (5, WithParensSelectWithParens <$> scale (`div` 2) arbitrary)
            ]
