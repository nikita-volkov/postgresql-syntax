-- |
-- Public surface of the @postgresql-syntax@ package.
--
-- Re-exports 'PostgresqlSyntax.IsAst', which provides the 'IsAst' class (the
-- per-type @parser@ \/ @toTextBuilder@ methods implemented in the
-- @PostgresqlSyntax.Ast.*@ node modules) together with the generic executors
-- 'parse', 'parseWithPosError' and 'toText'; and 'PostgresqlSyntax.Settings',
-- the parse\/render options ('mempty' is faithful standard Postgres,
-- 'PostgresqlSyntax.Settings.nullabilityMarkers' opts into the
-- 'PostgresqlSyntax.Ast.Typename' @?@ nullability markers).
module PostgresqlSyntax
  ( -- * AST
    module PostgresqlSyntax.IsAst,
    module PostgresqlSyntax.Ast,

    -- * Settings
    module PostgresqlSyntax.Settings,
  )
where

import PostgresqlSyntax.Ast
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Settings (Settings, nullabilityMarkers)
