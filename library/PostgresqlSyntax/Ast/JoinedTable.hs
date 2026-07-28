module PostgresqlSyntax.Ast.JoinedTable where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.JoinMeth
import {-# SOURCE #-} PostgresqlSyntax.Ast.TableRef (TableRef)
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder = \case
    InParensJoinedTable a -> renderInParens (toTextBuilder a)
    MethJoinedTable a b c -> toTextBuilder b <> " " <> toTextBuilder a <> " " <> toTextBuilder c
  parser =
    InParensJoinedTable
      <$> inParens parser
        <|> ( do
                b <- parser
                Parser.space1
                a <- parser
                Parser.space1
                c <- parser
                return (MethJoinedTable a b c)
            )

instance Qc.Arbitrary JoinedTable where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then MethJoinedTable <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.downscale Qc.arbitrary <*> Qc.downscale Qc.arbitrary
        else
          Qc.oneof
            [ InParensJoinedTable <$> Qc.scale (`div` 2) Qc.arbitrary,
              MethJoinedTable <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.downscale Qc.arbitrary <*> Qc.downscale Qc.arbitrary
            ]
