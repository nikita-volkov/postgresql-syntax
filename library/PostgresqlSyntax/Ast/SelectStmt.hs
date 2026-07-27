module PostgresqlSyntax.Ast.SelectStmt where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectNoParens (SelectNoParens)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
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
  arbitrary =
    Qc.oneof
      [ NoParensSelectStmt <$> Qc.scale (`div` 2) Qc.arbitrary,
        WithParensSelectStmt <$> Qc.scale (`div` 2) Qc.arbitrary
      ]
