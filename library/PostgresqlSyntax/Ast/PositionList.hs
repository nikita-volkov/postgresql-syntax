module PostgresqlSyntax.Ast.PositionList where

import {-# SOURCE #-} PostgresqlSyntax.Ast.BExpr (BExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

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
  parser = PositionList <$> parser <*> (space1 *> keyword "in" *> space1 *> parser)

instance Arbitrary PositionList where
  arbitrary = PositionList <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
