-- |
-- Public surface of the @postgresql-syntax@ package.
--
-- Re-exports 'PostgresqlSyntax.IsAst', which provides the 'IsAst' class (the
-- per-type @parser@ \/ @toTextBuilder@ methods implemented in the
-- @PostgresqlSyntax.Ast.*@ node modules) together with the generic executors
-- 'parse', 'parseWithPosError' and 'toText'.
module PostgresqlSyntax
  ( -- * AST
    module PostgresqlSyntax.IsAst,
    module PostgresqlSyntax.Ast,
  )
where

import PostgresqlSyntax.Ast
import PostgresqlSyntax.IsAst
