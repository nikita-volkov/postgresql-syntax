-- |
-- Parse Postgres SQL fragments into an AST and render that AST back to text.
--
-- Pick the 'PostgresqlSyntax.Ast' node type that corresponds to the syntax
-- you want to parse (e.g. 'PostgresqlSyntax.Ast.AExpr' for an expression),
-- then use 'parse' (or one of its error-detail variants) to get a value of
-- that type. Use 'toText' to go the other way and render an AST value back
-- into Postgres SQL syntax.
--
-- 'Settings' (built via 'nullabilityMarkers' and combined with
-- 'Semigroup'\/'Monoid') control optional parsing\/rendering behavior; pass
-- 'mempty' for standard Postgres syntax.
module PostgresqlSyntax
  ( -- * Parsing and rendering
    IsAst.IsAst (..),
    IsAst.toText,
    IsAst.parse,
    IsAst.parseWithPosError,
    IsAst.parseWithSourcePosError,

    -- * Settings
    Settings.Settings,
    Settings.nullabilityMarkers,

    -- * AST
    module PostgresqlSyntax.Ast,
  )
where

import PostgresqlSyntax.Ast
import qualified PostgresqlSyntax.IsAst as IsAst
import qualified PostgresqlSyntax.Settings as Settings
