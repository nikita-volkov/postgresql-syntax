module PostgresqlSyntax.Ast.TableRef where

import PostgresqlSyntax.IsAst (IsAst)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import Test.QuickCheck (Arbitrary)

data TableRef

instance Show TableRef

instance Eq TableRef

instance Ord TableRef

instance Data TableRef

instance IsAst TableRef

instance Arbitrary TableRef
