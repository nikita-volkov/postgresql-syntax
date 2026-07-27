module PostgresqlSyntax.Ast.OverlayList where

import HeadedMegaparsec
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

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
    space1
    keyword "placing"
    space1
    endHead
    b <- parser
    space1
    keyword "from"
    space1
    endHead
    c <- parser
    d <- optional (space1 *> keyword "for" *> space1 *> endHead *> parser)
    return (OverlayList a b c d)

instance Arbitrary OverlayList where
  arbitrary =
    OverlayList
      <$> scale (`div` 4) arbitrary
      <*> scale (`div` 4) arbitrary
      <*> scale (`div` 4) arbitrary
      <*> scale (`div` 4) arbitrary
