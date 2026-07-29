module PostgresqlSyntax.Ast.JoinedTable where

import PostgresqlSyntax.Ast.JoinMeth
import {-# SOURCE #-} PostgresqlSyntax.Ast.TableRef (TableRef)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder settings = \case
    InParensJoinedTable a -> TextBuilders.renderInParens (toTextBuilder settings a)
    MethJoinedTable a b c -> toTextBuilder settings b <> " " <> toTextBuilder settings a <> " " <> toTextBuilder settings c
  parser settings =
    InParensJoinedTable
      <$> Parsers.inParens (parser settings)
        <|> ( do
                b <- parser settings
                Parsers.space1
                a <- parser settings
                Parsers.space1
                c <- parser settings
                return (MethJoinedTable a b c)
            )

instance Qc.Arbitrary JoinedTable where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then MethJoinedTable <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary <*> Gens.downscale Qc.arbitrary
        else
          Qc.oneof
            [ InParensJoinedTable <$> Gens.downscale Qc.arbitrary,
              MethJoinedTable <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary <*> Gens.downscale Qc.arbitrary
            ]
