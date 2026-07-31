module PostgresqlSyntax.Ast.JoinedTable where

import PostgresqlSyntax.Algebra (IsAst)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import Test.QuickCheck (Arbitrary)

data JoinedTable

instance Show JoinedTable

instance Eq JoinedTable

instance Ord JoinedTable

instance Data JoinedTable

instance IsAst JoinedTable

instance Arbitrary JoinedTable
