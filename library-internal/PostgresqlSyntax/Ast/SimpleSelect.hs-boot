module PostgresqlSyntax.Ast.SimpleSelect where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectClause (SelectClause)
import PostgresqlSyntax.Algebra (IsAst)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Parser, Show)
import PostgresqlSyntax.Settings (Settings)
import Test.QuickCheck (Arbitrary)

data SimpleSelect

instance Show SimpleSelect

instance Eq SimpleSelect

instance Ord SimpleSelect

instance Data SimpleSelect

instance IsAst SimpleSelect

instance Arbitrary SimpleSelect

selectClauseBase :: Settings -> Parser SelectClause

extendSelectClause :: Settings -> SelectClause -> Parser SelectClause
