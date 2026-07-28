module PostgresqlSyntax.Ast.Columnref where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident (Ident, colId)
import PostgresqlSyntax.Ast.Indirection
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- columnref:
--   | ColId
--   | ColId indirection
-- @
data Columnref = Columnref Ident (Maybe Indirection)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Columnref where
  toTextBuilder (Columnref a b) = toTextBuilder a <> foldMap toTextBuilder b
  parser = do
    a <- Parser.wrapToHead colId
    Parser.endHead
    b <- optional (Parser.space *> parser)
    return (Columnref a b)

instance Qc.Arbitrary Columnref where
  shrink = Qc.genericShrink
  arbitrary = Columnref <$> arbitrary <*> Qc.scale (`div` 2) arbitrary
