module PostgresqlSyntax.Ast.JoinedTable where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.JoinMeth
import {-# SOURCE #-} PostgresqlSyntax.Ast.TableRef (TableRef, joinedTableParser, renderJoinedTable)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
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

-- |
-- Delegates to 'PostgresqlSyntax.Ast.TableRef.renderJoinedTable'\/
-- 'PostgresqlSyntax.Ast.TableRef.joinedTableParser' rather than combining
-- 'TableRef'\'s and 'JoinMeth'\'s own instances directly: a bare @table_ref@
-- parse is greedy — it absorbs any trailing @CROSS JOIN@\/@JOIN@\/@NATURAL
-- JOIN@ continuation into itself (see 'PostgresqlSyntax.Ast.TableRef'\'s
-- @recur@) — so parsing this type's own @b@ field with the plain exported
-- 'PostgresqlSyntax.Ast.TableRef.parser' would always swallow the @a@\/@c@
-- that's meant to follow it, and 'JoinMeth'\'s own renderer places a
-- 'PostgresqlSyntax.Ast.JoinQual' immediately after the @JOIN@ keyword
-- rather than after @c@ (see its own doc for why). 'TableRef' is the only
-- module with both this type and 'TableRef' in scope non-abstractly at
-- once, so it hosts the one correct, round-trippable implementation.
instance IsAst JoinedTable where
  toTextBuilder = renderJoinedTable
  parser = joinedTableParser

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
