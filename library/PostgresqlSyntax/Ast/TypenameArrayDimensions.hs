module PostgresqlSyntax.Ast.TypenameArrayDimensions where

import HeadedMegaparsec hiding (string)
import PostgresqlSyntax.Ast.ArrayBounds
import PostgresqlSyntax.Ast.Iconst
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, option, some, try)

-- |
-- ==== References
-- @
-- Part of the Typename specification responsible for the choice between the following:
--   | opt_array_bounds
--   | ARRAY '[' Iconst ']'
--   | ARRAY
-- @
data TypenameArrayDimensions
  = BoundsTypenameArrayDimensions ArrayBounds
  | ExplicitTypenameArrayDimensions (Maybe Iconst)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TypenameArrayDimensions where
  toTextBuilder = \case
    BoundsTypenameArrayDimensions a -> toTextBuilder a
    ExplicitTypenameArrayDimensions a -> " ARRAY" <> foldMap (renderInBrackets . toTextBuilder) a
  parser =
    asum
      [ do
          space1
          keyword "array"
          endHead
          ExplicitTypenameArrayDimensions <$> optional (space *> inBrackets parser),
        BoundsTypenameArrayDimensions <$> (space *> parser)
      ]

instance Arbitrary TypenameArrayDimensions where
  arbitrary =
    oneof
      [ BoundsTypenameArrayDimensions <$> arbitrary,
        ExplicitTypenameArrayDimensions <$> arbitrary
      ]
