module PostgresqlSyntax.Ast.SelectNoParens where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectClause (SelectClause)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import PostgresqlSyntax.IsAst (IsAst)
import PostgresqlSyntax.Prelude (Data, Either, Eq, Maybe, Ord, Parser, Show)
import PostgresqlSyntax.Settings (Settings)
import Test.QuickCheck (Arbitrary)

data SelectNoParens

instance Show SelectNoParens

instance Eq SelectNoParens

instance Ord SelectNoParens

instance Data SelectNoParens

instance IsAst SelectNoParens

instance Arbitrary SelectNoParens

unparenthesizedSelectNoParensParser :: Settings -> Parser SelectNoParens

afterSelectWithParensClauseParser :: Settings -> SelectWithParens -> Parser (Either SelectWithParens SelectNoParens)

refineToSelectWithParens :: SelectNoParens -> Maybe SelectWithParens
