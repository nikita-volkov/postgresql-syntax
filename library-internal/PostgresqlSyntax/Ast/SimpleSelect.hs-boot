module PostgresqlSyntax.Ast.SimpleSelect where

import PostgresqlSyntax.Algebra (IsAst, LeftRecursion, LeftRecursive)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectClause (SelectClause)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import Test.QuickCheck (Arbitrary)

data SimpleSelect

data SelectChainLink

instance Show SimpleSelect

instance Eq SimpleSelect

instance Ord SimpleSelect

instance Data SimpleSelect

instance IsAst SimpleSelect

instance Arbitrary SimpleSelect

instance LeftRecursive SimpleSelect

instance LeftRecursion SelectClause SimpleSelect SelectChainLink
