module PostgresqlSyntax.Ast.FrameExtent where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.FrameBound
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
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
  toTextBuilder settings = \case
    SingularFrameExtent a -> toTextBuilder settings a
    BetweenFrameExtent a b -> "BETWEEN " <> toTextBuilder settings a <> " AND " <> toTextBuilder settings b
  parser settings =
    BetweenFrameExtent
      <$> (Parsers.keyword "between" *> Parsers.space1 *> Parser.endHead *> parser settings <* Parsers.space1 <* Parsers.keyword "and" <* Parsers.space1)
      <*> parser settings
        <|> SingularFrameExtent
      <$> parser settings

instance Qc.Arbitrary FrameExtent where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ SingularFrameExtent <$> Qc.arbitrary,
        BetweenFrameExtent <$> Qc.arbitrary <*> Qc.arbitrary
      ]
