module PostgresqlSyntax.Ast.TableRef where

import PostgresqlSyntax.Algebra (IsAst, LeftRecursive, Refines)
import {-# SOURCE #-} PostgresqlSyntax.Ast.JoinedTable (JoinedTable)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import Test.QuickCheck (Arbitrary)

data TableRef

instance Show TableRef

instance Eq TableRef

instance Ord TableRef

instance Data TableRef

instance IsAst TableRef

instance Arbitrary TableRef

instance Refines JoinedTable TableRef

instance LeftRecursive TableRef
