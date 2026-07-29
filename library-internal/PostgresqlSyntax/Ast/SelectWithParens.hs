module PostgresqlSyntax.Ast.SelectWithParens
  ( SelectWithParens (..),
    withParensSelectWithParensInner,
  )
where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectNoParens (SelectNoParens, afterSelectWithParensClause, trivialSelectWithParensWrapper, unparenthesizedSelectNoParens)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
  toTextBuilder settings =
    TextBuilders.renderInParens . \case
      NoParensSelectWithParens a -> toTextBuilder settings a
      WithParensSelectWithParens a -> toTextBuilder settings a

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
  parser settings = Parsers.inParens selectWithParensBody
    where
      selectWithParensBody =
        asum
          [ do
              a <- Parser.wrapToHead (parser settings)
              either WithParensSelectWithParens NoParensSelectWithParens <$> afterSelectWithParensClause settings a,
            NoParensSelectWithParens <$> unparenthesizedSelectNoParens settings
          ]

instance Qc.Arbitrary SelectWithParens where
  shrink = fmap canonicalize . Qc.genericShrink
  arbitrary =
    canonicalize
      <$> Qc.frequency
        [ (3, NoParensSelectWithParens <$> Gens.downscale Qc.arbitrary),
          (1, WithParensSelectWithParens <$> Gens.downscale Qc.arbitrary)
        ]

-- |
-- Collapses the non-canonical @NoParensSelectWithParens@ shape described
-- above to the @WithParensSelectWithParens@ shape the parser actually
-- produces for it. Both 'arbitrary' and 'shrink' can otherwise construct
-- the non-canonical shape (shrinking the inner 'SelectNoParens' toward
-- @Nothing@s is exactly how it arises), which renders fine but parses back
-- to a different, canonical value and so breaks the roundtrip property.
canonicalize :: SelectWithParens -> SelectWithParens
canonicalize = \case
  NoParensSelectWithParens a
    | Just c <- trivialSelectWithParensWrapper a -> WithParensSelectWithParens c
  other -> other

-- |
-- If a 'SelectWithParens' is the @WithParensSelectWithParens@ wrapping of
-- another one, returns the wrapped value. Exposed for modules that can
-- only see 'SelectWithParens' via its @hs-boot@ (which keeps it abstract
-- to break an import cycle) — see "PostgresqlSyntax.Ast.InExpr", which
-- needs it to canonicalize a @select_with_parens@\/@expr_list@ ambiguity
-- analogous to the one described above.
withParensSelectWithParensInner :: SelectWithParens -> Maybe SelectWithParens
withParensSelectWithParensInner = \case
  WithParensSelectWithParens a -> Just a
  _ -> Nothing
