-- |
-- Public surface of the @postgresql-syntax@ package.
--
-- Re-exports 'PostgresqlSyntax.IsAst', which provides the 'IsAst' class (the
-- per-type @parser@ \/ @toTextBuilder@ methods implemented in the
-- @PostgresqlSyntax.Ast.*@ node modules) together with the generic executors
-- 'run', 'runWithPosError', 'atEnd' and 'toText'. For the AST types themselves
-- see "PostgresqlSyntax.Ast".
module PostgresqlSyntax
  ( module PostgresqlSyntax.IsAst,
  )
where

import PostgresqlSyntax.IsAst
