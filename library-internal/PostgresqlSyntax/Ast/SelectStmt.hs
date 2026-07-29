module PostgresqlSyntax.Ast.SelectStmt where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectNoParens (SelectNoParens)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- SelectStmt:
--   |  select_no_parens
--   |  select_with_parens
-- @
data SelectStmt
  = NoParensSelectStmt SelectNoParens
  | WithParensSelectStmt SelectWithParens
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SelectStmt where
  toTextBuilder = \case
    NoParensSelectStmt a -> toTextBuilder a
    WithParensSelectStmt a -> toTextBuilder a
  parser = NoParensSelectStmt <$> parser <|> WithParensSelectStmt <$> parser

instance Qc.Arbitrary SelectStmt where
  shrink = Qc.genericShrink

  -- @WithParensSelectStmt@ is unreachable via this type's own @parser@:
  -- @NoParensSelectStmt@'s alternative is tried first, and
  -- 'PostgresqlSyntax.Ast.SelectClause' (reachable from any
  -- @select_no_parens@ with every other clause absent) always accepts a
  -- parenthesized select too — so any @'(' select ')'@ text always parses
  -- as @NoParensSelectStmt (SelectNoParens Nothing (WithParensSelectClause
  -- _) Nothing Nothing Nothing)@, never as a bare @WithParensSelectStmt@.
  -- Generating the latter would therefore never round-trip.
  arbitrary = NoParensSelectStmt <$> Gens.downscale Qc.arbitrary
