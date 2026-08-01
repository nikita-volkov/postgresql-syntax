module PostgresqlSyntax.Ast.JoinedTable
  ( JoinedTable (..),
    inParensJoinedTable,
    JoinedTableExtension (..),
  )
where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.JoinQual
import PostgresqlSyntax.Ast.JoinType
import {-# SOURCE #-} PostgresqlSyntax.Ast.TableRef (TableRef)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import PostgresqlSyntax.Settings (Settings)
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
-- @
data JoinedTable
  = InParensJoinedTable JoinedTable
  | CrossJoinedTable TableRef TableRef
  | QualJoinedTable TableRef (Maybe JoinType) TableRef JoinQual
  | NaturalJoinedTable TableRef (Maybe JoinType) TableRef
  deriving (Show, Generic, Eq, Ord, Data)

-- |
-- Parsing delegates to 'PostgresqlSyntax.Algebra.parseExtended' over the
-- 'PostgresqlSyntax.Algebra.LeftRecursion' instance hosted in
-- "PostgresqlSyntax.Ast.TableRef" (see its own doc) — a bare @table_ref@
-- parse is greedy, absorbing any trailing @CROSS JOIN@\/@JOIN@\/@NATURAL
-- JOIN@ continuation into itself, so a @joined_table@ is never reachable
-- as a bare, zero-extension 'TableRef'; it always needs at least one.
instance IsAst JoinedTable where
  toTextBuilder settings = \case
    InParensJoinedTable a -> TextBuilders.renderInParens (toTextBuilder settings a)
    CrossJoinedTable a b -> toTextBuilder settings a <> " CROSS JOIN " <> toTextBuilder settings b
    QualJoinedTable a b c d -> toTextBuilder settings a <> TextBuilders.suffixMaybe (toTextBuilder settings) b <> " JOIN " <> toTextBuilder settings c <> " " <> toTextBuilder settings d
    NaturalJoinedTable a b c -> toTextBuilder settings a <> " NATURAL" <> TextBuilders.suffixMaybe (toTextBuilder settings) b <> " JOIN " <> toTextBuilder settings c

  parser settings = parseExtended @TableRef settings <|> inParensJoinedTable settings

-- ==== References
-- @
--   | '(' joined_table ')'
-- @
inParensJoinedTable :: Settings -> Parser JoinedTable
inParensJoinedTable settings = InParensJoinedTable <$> Parsers.inParens (parser settings)

instance Qc.Arbitrary JoinedTable where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then joined
        else Qc.oneof [InParensJoinedTable <$> Gens.downscale Qc.arbitrary, joined]
    where
      joined =
        Qc.oneof
          [ CrossJoinedTable <$> Gens.downscale Qc.arbitrary <*> Gens.downscale Qc.arbitrary,
            QualJoinedTable <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary <*> Gens.downscale Qc.arbitrary <*> Qc.arbitrary,
            NaturalJoinedTable <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary <*> Gens.downscale Qc.arbitrary
          ]

-- |
-- A 'JoinedTable' with its left operand removed — the 'item' half of
-- 'PostgresqlSyntax.Ast.TableRef'\'s
-- 'PostgresqlSyntax.Algebra.LeftRecursion' instance (see its own doc).
-- Mirrors 'JoinedTable'\'s three join-bearing constructors one-for-one,
-- each missing its leading 'TableRef'.
data JoinedTableExtension
  = CrossJoinedTableExtension TableRef
  | QualJoinedTableExtension (Maybe JoinType) TableRef JoinQual
  | NaturalJoinedTableExtension (Maybe JoinType) TableRef
