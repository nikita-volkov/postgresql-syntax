module PostgresqlSyntax.Ast.FrameExtent where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.FrameBound
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude

-- |
-- ==== References
-- @
-- frame_extent:
--   |  frame_bound
--   |  BETWEEN frame_bound AND frame_bound
-- @
data FrameExtent = SingularFrameExtent FrameBound | BetweenFrameExtent FrameBound FrameBound
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FrameExtent where
  toTextBuilder = \case
    SingularFrameExtent a -> toTextBuilder a
    BetweenFrameExtent a b -> "BETWEEN " <> toTextBuilder a <> " AND " <> toTextBuilder b
  parser =
    BetweenFrameExtent
      <$> (keyword "between" *> space1 *> endHead *> parser <* space1 <* keyword "and" <* space1)
      <*> parser
      <|> SingularFrameExtent
      <$> parser

instance Arbitrary FrameExtent where
  arbitrary =
    oneof
      [ SingularFrameExtent <$> arbitrary,
        BetweenFrameExtent <$> arbitrary <*> arbitrary
      ]
