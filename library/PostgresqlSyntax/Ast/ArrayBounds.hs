module PostgresqlSyntax.Ast.ArrayBounds where

import PostgresqlSyntax.Ast.Iconst
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- opt_array_bounds:
--   | opt_array_bounds '[' Iconst ']'
--   | opt_array_bounds '[' ']'
--   | EMPTY
-- @
newtype ArrayBounds = ArrayBounds (NonEmpty (Maybe Iconst))
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ArrayBounds where
  toTextBuilder (ArrayBounds a) = TextBuilders.spaceNonEmpty (TextBuilders.renderInBrackets . foldMap toTextBuilder) a
  parser = ArrayBounds <$> Parsers.sep1 Parsers.space (Parsers.inBrackets (optional parser))

instance Qc.Arbitrary ArrayBounds where
  shrink = Qc.genericShrink
  arbitrary = ArrayBounds <$> Qc.nonEmptyUpTo 3 (Qc.oneof [pure Nothing, Just <$> Qc.arbitrary])
