module PostgresqlSyntax.Ast.SimpleSelect where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectClause (SelectClause)
import PostgresqlSyntax.Algebra (IsAst, LeftRecursion)
import PostgresqlSyntax.Ast.SelectBinOp (SelectBinOp)
import PostgresqlSyntax.Prelude (Bool, Data, Eq, Maybe, Ord, Show)
import Test.QuickCheck (Arbitrary)

data SimpleSelect

instance Show SimpleSelect

instance Eq SimpleSelect

instance Ord SimpleSelect

instance Data SimpleSelect

instance IsAst SimpleSelect

instance Arbitrary SimpleSelect

instance LeftRecursion SelectClause SimpleSelect (SelectBinOp, Maybe Bool, SelectClause)
