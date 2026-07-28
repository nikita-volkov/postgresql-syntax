module PostgresqlSyntax.Ast.SetTarget where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
  toTextBuilder (SetTarget a b) = toTextBuilder a <> TextBuilders.suffixMaybe toTextBuilder b
  parser = do
    a <- colId
    Parser.endHead
    b <- optional (Parsers.space1 *> parser)
    return (SetTarget a b)

instance Qc.Arbitrary SetTarget where
  shrink = Qc.genericShrink
  arbitrary = SetTarget <$> arbitrary <*> Qc.terminatingMaybe arbitrary
