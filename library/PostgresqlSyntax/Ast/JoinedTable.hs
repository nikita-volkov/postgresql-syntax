module PostgresqlSyntax.Ast.JoinedTable where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.JoinMeth
import {-# SOURCE #-} PostgresqlSyntax.Ast.TableRef (TableRef)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- | '(' joined_table ')'
-- | table_ref CROSS JOIN table_ref
-- | table_ref join_type JOIN table_ref join_qual
-- | table_ref JOIN table_ref join_qual
-- | table_ref NATURAL join_type JOIN table_ref
-- | table_ref NATURAL JOIN table_ref
--
-- The options are covered by the `JoinMeth` type.
-- @
--
-- See 'PostgresqlSyntax.Ast.JoinMeth' for why this type's own 'IsAst'
-- instance isn't what 'PostgresqlSyntax.Ast.TableRef' actually uses to
-- parse\/render joined tables.
data JoinedTable
  = InParensJoinedTable JoinedTable
  | MethJoinedTable JoinMeth TableRef TableRef
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst JoinedTable where
  toTextBuilder = \case
    InParensJoinedTable a -> renderInParens (toTextBuilder a)
    MethJoinedTable a b c -> toTextBuilder b <> " " <> toTextBuilder a <> " " <> toTextBuilder c
  parser =
    InParensJoinedTable
      <$> inParens parser
      <|> (MethJoinedTable <$> parser <*> parser <*> (space1 *> parser))

instance Arbitrary JoinedTable where
  arbitrary =
    sized $ \n ->
      if n <= 1
        then MethJoinedTable <$> scale (`div` 2) arbitrary <*> arbitrary <*> arbitrary
        else
          oneof
            [ InParensJoinedTable <$> scale (`div` 2) arbitrary,
              MethJoinedTable <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
            ]
