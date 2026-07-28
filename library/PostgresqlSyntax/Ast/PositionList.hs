module PostgresqlSyntax.Ast.PositionList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.BExpr (BExpr)
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- position_list:
--   | b_expr IN_P b_expr
--   | EMPTY
-- @
data PositionList = PositionList BExpr BExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst PositionList where
  toTextBuilder (PositionList a b) = toTextBuilder a <> " IN " <> toTextBuilder b
  parser = PositionList <$> parser <*> (Parser.space1 *> keyword "in" *> Parser.space1 *> parser)

instance Qc.Arbitrary PositionList where
  shrink = Qc.genericShrink
  arbitrary = PositionList <$> Qc.downscale arbitrary <*> Qc.downscale arbitrary
