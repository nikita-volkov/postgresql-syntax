module PostgresqlSyntax.Ast.SimpleSelect where

import PostgresqlSyntax.Algebra (IsAst, LeftRecursion, LeftRecursive)
import PostgresqlSyntax.Ast.SelectBinOp (SelectBinOp)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectClause (SelectClause)
import PostgresqlSyntax.Prelude (Bool, Data, Eq, Maybe, Ord, Show)
import Test.QuickCheck (Arbitrary)

data SimpleSelect

instance Show SimpleSelect

instance Eq SimpleSelect

instance Ord SimpleSelect

instance Data SimpleSelect

instance IsAst SimpleSelect

instance Arbitrary SimpleSelect

instance LeftRecursive SimpleSelect

instance LeftRecursion SelectClause SimpleSelect (SelectBinOp, Maybe Bool, SelectClause)
