module PostgresqlSyntax.Ast.SimpleSelect where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectClause (SelectClause)
import PostgresqlSyntax.IsAst (IsAst)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Parser, Show)
import Test.QuickCheck (Arbitrary)

data SimpleSelect

instance Show SimpleSelect

instance Eq SimpleSelect

instance Ord SimpleSelect

instance Data SimpleSelect

instance IsAst SimpleSelect

instance Arbitrary SimpleSelect

-- | The non-recursive @simple_select@ base cases (@SELECT ...@\/@TABLE
-- ...@\/@VALUES ...@, excluding the @select_clause BINOP select_clause@
-- extension). Needed by "PostgresqlSyntax.Ast.SelectNoParens", which
-- shares this exact grammar fragment.
baseSimpleSelect :: Parser SimpleSelect

-- | @select_clause@'s non-extended base (a bare 'SimpleSelect' or a
-- parenthesized select), before any @UNION@\/@INTERSECT@\/@EXCEPT@ suffix.
selectClauseBase :: Parser SelectClause

-- | Extends an already-parsed 'SelectClause' with zero or more
-- @UNION@\/@INTERSECT@\/@EXCEPT@ suffixes, each producing a bigger
-- 'SimpleSelect' via @BinSimpleSelect@ (the only place that can construct
-- one, hence why this lives here rather than in
-- "PostgresqlSyntax.Ast.SelectClause" itself).
extendSelectClause :: SelectClause -> Parser SelectClause
