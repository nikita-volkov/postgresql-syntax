module PostgresqlSyntax.Ast.ArrayBounds where

import qualified Data.List.NonEmpty as NonEmpty
import PostgresqlSyntax.Ast.Iconst
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, option, some, try)

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
  toTextBuilder (ArrayBounds a) = spaceNonEmpty (renderInBrackets . foldMap toTextBuilder) a
  parser = ArrayBounds <$> sep1 space (inBrackets (optional parser))

instance Arbitrary ArrayBounds where
  arbitrary = do
    len <- choose (1, 4)
    ArrayBounds . NonEmpty.fromList <$> vectorOf len (oneof [pure Nothing, Just <$> arbitrary])
