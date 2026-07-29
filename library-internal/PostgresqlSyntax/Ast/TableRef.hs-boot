module PostgresqlSyntax.Ast.TableRef where

import {-# SOURCE #-} PostgresqlSyntax.Ast.JoinedTable (JoinedTable)
import PostgresqlSyntax.IsAst (IsAst)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Parser, Show, TextBuilder)
import PostgresqlSyntax.Settings (Settings)
import Test.QuickCheck (Arbitrary)

data TableRef

instance Show TableRef

instance Eq TableRef

instance Ord TableRef

instance Data TableRef

instance IsAst TableRef

instance Arbitrary TableRef

-- | See "PostgresqlSyntax.Ast.TableRef" for the full documentation. The
-- actual rendering logic for 'PostgresqlSyntax.Ast.JoinedTable', exposed so
-- that module's own 'IsAst' instance can delegate to it instead of
-- maintaining a second, subtly different copy.
renderJoinedTable :: Settings -> JoinedTable -> TextBuilder

-- | See "PostgresqlSyntax.Ast.TableRef" for the full documentation. The
-- actual parsing logic for 'PostgresqlSyntax.Ast.JoinedTable', exposed for
-- the same reason as 'renderJoinedTable'.
joinedTableParser :: Settings -> Parser JoinedTable
