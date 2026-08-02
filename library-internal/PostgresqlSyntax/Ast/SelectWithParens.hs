module PostgresqlSyntax.Ast.SelectWithParens
  ( SelectWithParens (..),
  )
where

import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectNoParens (SelectNoParens)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
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

  -- ==== Canonical shape
  --
  -- Because @select_with_parens@ (@'(' select_with_parens ')'@) and
  -- @'(' select_no_parens ')'@ both parse @((select 1))@, there are two
  -- possible representations: @WithParensSelectWithParens@ wrapping the
  -- inner parenthesised select, or @NoParensSelectWithParens@ of a
  -- @SelectNoParens@ whose clause is that inner select. Both render back
  -- to the same text. __The first (@WithParensSelectWithParens@) is
  -- canonical.__ The 'Canonicalizes' instance normalizes shrunk or generated
  -- values to this shape.
  parser settings = Parsers.inParens (canonicalize . NoParensSelectWithParens <$> parser settings)

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
instance Canonicalizes SelectWithParens where
  canonicalize = \case
    NoParensSelectWithParens a
      | Just c <- project @SelectWithParens a -> WithParensSelectWithParens c
    other -> other

instance Refines SelectWithParens SelectWithParens where
  embed = WithParensSelectWithParens
  project = \case
    WithParensSelectWithParens a -> Just a
    _ -> Nothing
