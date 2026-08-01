module PostgresqlSyntax.Ast.JoinedTable
  ( JoinedTable (..),
    inParensJoinedTable,
  )
where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.JoinMeth
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

-- |
-- Rendering combines 'TableRef'\'s and 'JoinMeth'\'s constructors directly
-- rather than delegating to either's own 'IsAst' instance: 'JoinMeth'\'s own
-- renderer places a 'PostgresqlSyntax.Ast.JoinQual' immediately after the
-- @JOIN@ keyword rather than after the right operand (see its own doc for
-- why), so it isn't what belongs here.
--
-- Parsing delegates to 'PostgresqlSyntax.Algebra.parseExtended' over the
-- 'PostgresqlSyntax.Algebra.LeftRecursion' instance hosted in
-- "PostgresqlSyntax.Ast.TableRef" (see its own doc) — a bare @table_ref@
-- parse is greedy, absorbing any trailing @CROSS JOIN@\/@JOIN@\/@NATURAL
-- JOIN@ continuation into itself, so a @joined_table@ is never reachable
-- as a bare, zero-extension 'TableRef'; it always needs at least one.
instance IsAst JoinedTable where
  toTextBuilder settings = \case
    InParensJoinedTable a -> TextBuilders.renderInParens (toTextBuilder settings a)
    MethJoinedTable a b c -> case a of
      CrossJoinMeth -> toTextBuilder settings b <> " CROSS JOIN " <> toTextBuilder settings c
      QualJoinMeth d e -> toTextBuilder settings b <> TextBuilders.suffixMaybe (toTextBuilder settings) d <> " JOIN " <> toTextBuilder settings c <> " " <> toTextBuilder settings e
      NaturalJoinMeth d -> toTextBuilder settings b <> " NATURAL" <> TextBuilders.suffixMaybe (toTextBuilder settings) d <> " JOIN " <> toTextBuilder settings c

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
        then MethJoinedTable <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary <*> Gens.downscale Qc.arbitrary
        else
          Qc.oneof
            [ InParensJoinedTable <$> Gens.downscale Qc.arbitrary,
              MethJoinedTable <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary <*> Gens.downscale Qc.arbitrary
            ]
