module PostgresqlSyntax.Ast.TypenameArrayDimensions where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ArrayBounds
import PostgresqlSyntax.Ast.Iconst
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
  toTextBuilder settings = \case
    BoundsTypenameArrayDimensions a -> toTextBuilder settings a
    ExplicitTypenameArrayDimensions a -> " ARRAY" <> foldMap (TextBuilders.renderInBrackets . toTextBuilder settings) a
  parser settings =
    asum
      [ do
          Parsers.space1
          Parsers.keyword "array"
          Parser.endHead
          ExplicitTypenameArrayDimensions <$> optional (Parsers.space *> Parsers.inBrackets (parser settings)),
        BoundsTypenameArrayDimensions <$> (Parsers.space *> parser settings)
      ]

instance Qc.Arbitrary TypenameArrayDimensions where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ BoundsTypenameArrayDimensions <$> Qc.arbitrary,
        ExplicitTypenameArrayDimensions <$> Qc.arbitrary
      ]
