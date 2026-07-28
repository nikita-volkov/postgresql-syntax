module PostgresqlSyntax.Ast.OverlayList where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  toTextBuilder (OverlayList a b c d) =
    toTextBuilder a
      <> " PLACING "
      <> toTextBuilder b
      <> " FROM "
      <> toTextBuilder c
      <> suffixMaybe (mappend "FOR " . toTextBuilder) d
    where
      suffixMaybe f = foldMap (mappend " " . f)
  parser = do
    a <- parser
    Parser.space1
    keyword "placing"
    Parser.space1
    Parser.endHead
    b <- parser
    Parser.space1
    keyword "from"
    Parser.space1
    Parser.endHead
    c <- parser
    d <- optional (Parser.space1 *> keyword "for" *> Parser.space1 *> Parser.endHead *> parser)
    return (OverlayList a b c d)

instance Qc.Arbitrary OverlayList where
  shrink = Qc.genericShrink
  arbitrary =
    OverlayList
      <$> Qc.scale (`div` 4) Qc.arbitrary
      <*> Qc.scale (`div` 4) Qc.arbitrary
      <*> Qc.scale (`div` 4) Qc.arbitrary
      <*> Qc.scale (`div` 4) Qc.arbitrary
