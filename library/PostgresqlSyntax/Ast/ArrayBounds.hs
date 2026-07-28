module PostgresqlSyntax.Ast.ArrayBounds where

import PostgresqlSyntax.Ast.Iconst
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder (ArrayBounds a) = spaceNonEmpty (renderInBrackets . foldMap toTextBuilder) a
  parser = ArrayBounds <$> Parser.sep1 Parser.space (inBrackets (optional parser))

instance Qc.Arbitrary ArrayBounds where
  shrink = Qc.genericShrink
  arbitrary = ArrayBounds <$> Qc.nonEmptyUpTo 3 (Qc.oneof [pure Nothing, Just <$> Qc.arbitrary])
