module PostgresqlSyntax.Ast.SelectNoParens where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectClause (SelectClause)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import PostgresqlSyntax.IsAst (IsAst)
import PostgresqlSyntax.Prelude (Data, Either, Eq, Maybe, Ord, Parser, Show)
import Test.QuickCheck (Arbitrary)

data SelectNoParens

instance Show SelectNoParens

instance Eq SelectNoParens

instance Ord SelectNoParens

instance Data SelectNoParens

instance IsAst SelectNoParens

instance Arbitrary SelectNoParens

-- | 'PostgresqlSyntax.Ast.SelectNoParens.parser' restricted to the forms
-- that do not begin with @(@. Needed by
-- "PostgresqlSyntax.Ast.SelectWithParens", where the paren-leading form is
-- already covered by the branch that shares the nested
-- @select_with_parens@ parse — admitting it here again would reintroduce
-- the doubling that parser is written to avoid.
unparenthesizedSelectNoParens :: Parser SelectNoParens

-- | The remainder of a @select_no_parens@, resumed from an already-parsed
-- 'SelectClause' (and optional leading @with_clause@). Split out so that
-- "PostgresqlSyntax.Ast.SelectWithParens", which has to parse that base
-- itself to decide between the two parenthesized forms, can continue
-- without parsing it again.
selectNoParensAfterClause :: Maybe WithClause -> SelectClause -> Parser SelectNoParens

-- | See the real module's doc for what this decides.
afterSelectWithParensClause :: SelectWithParens -> Parser (Either SelectWithParens SelectNoParens)

-- | See the real module's doc for what this decides.
trivialSelectWithParensWrapper :: SelectNoParens -> Maybe SelectWithParens
