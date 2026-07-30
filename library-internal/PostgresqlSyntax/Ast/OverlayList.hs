module PostgresqlSyntax.Ast.OverlayList where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- overlay_list:
--   | a_expr overlay_placing substr_from substr_for
--   | a_expr overlay_placing substr_from
-- @
--
-- @overlay_placing@\/@substr_from@\/@substr_for@ are bare aliases to
-- 'PostgresqlSyntax.Ast.AExpr' (@PLACING a_expr@\/@FROM a_expr@\/@FOR
-- a_expr@ respectively); their tiny keyword-prefix wrapping is inlined here
-- rather than named, since 'PostgresqlSyntax.Ast.SubstrListFromFor' has its
-- own (differently-scoped) copy of the @FROM@\/@FOR@ half.
data OverlayList = OverlayList AExpr AExpr AExpr (Maybe AExpr)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OverlayList where
  toTextBuilder settings (OverlayList a b c d) =
    toTextBuilder settings a
      <> " PLACING "
      <> toTextBuilder settings b
      <> " FROM "
      <> toTextBuilder settings c
      <> TextBuilders.suffixMaybe (mappend "FOR " . toTextBuilder settings) d
  parser settings = do
    a <- parser settings
    Parsers.space1
    Parsers.keyword "placing"
    Parsers.space1
    Parser.endHead
    b <- parser settings
    Parsers.space1
    Parsers.keyword "from"
    Parsers.space1
    Parser.endHead
    c <- parser settings
    d <- optional (Parsers.space1 *> Parsers.keyword "for" *> Parsers.space1 *> Parser.endHead *> parser settings)
    return (OverlayList a b c d)

instance Qc.Arbitrary OverlayList where
  shrink = Qc.genericShrink
  arbitrary =
    OverlayList
      <$> Gens.downscale Qc.arbitrary
      <*> Gens.downscale Qc.arbitrary
      <*> Gens.downscale Qc.arbitrary
      <*> Gens.terminatingMaybe (Gens.downscale Qc.arbitrary)
