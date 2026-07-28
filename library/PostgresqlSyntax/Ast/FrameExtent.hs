module PostgresqlSyntax.Ast.FrameExtent where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.FrameBound
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
      <$> (keyword "between" *> Parser.space1 *> Parser.endHead *> parser <* Parser.space1 <* keyword "and" <* Parser.space1)
      <*> parser
        <|> SingularFrameExtent
      <$> parser

instance Qc.Arbitrary FrameExtent where
  arbitrary =
    Qc.oneof
      [ SingularFrameExtent <$> Qc.arbitrary,
        BetweenFrameExtent <$> Qc.arbitrary <*> Qc.arbitrary
      ]
