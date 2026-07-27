module PostgresqlSyntax.Ast.SetTarget where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- set_target:
--   | ColId opt_indirection
-- @
data SetTarget = SetTarget Ident (Maybe Indirection)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SetTarget where
  toTextBuilder (SetTarget a b) = toTextBuilder a <> suffixMaybe toTextBuilder b
  parser = do
    a <- colId
    Parser.endHead
    b <- optional (Parser.space1 *> parser)
    return (SetTarget a b)

instance Qc.Arbitrary SetTarget where
  arbitrary = SetTarget <$> arbitrary <*> Qc.scale (`div` 2) arbitrary
