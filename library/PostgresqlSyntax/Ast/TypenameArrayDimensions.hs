module PostgresqlSyntax.Ast.TypenameArrayDimensions where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ArrayBounds
import PostgresqlSyntax.Ast.Iconst
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.Helpers.TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
          Parser.space1
          keyword "array"
          Parser.endHead
          ExplicitTypenameArrayDimensions <$> optional (Parser.space *> inBrackets parser),
        BoundsTypenameArrayDimensions <$> (Parser.space *> parser)
      ]

instance Qc.Arbitrary TypenameArrayDimensions where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ BoundsTypenameArrayDimensions <$> Qc.arbitrary,
        ExplicitTypenameArrayDimensions <$> Qc.arbitrary
      ]
