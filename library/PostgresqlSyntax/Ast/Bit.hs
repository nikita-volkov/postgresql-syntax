module PostgresqlSyntax.Ast.Bit where

import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.OptVarying
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.Helpers.TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- Bit:
--   | BitWithLength
--   | BitWithoutLength
-- ConstBit:
--   | BitWithLength
--   | BitWithoutLength
-- BitWithLength:
--   | BIT opt_varying '(' expr_list ')'
-- BitWithoutLength:
--   | BIT opt_varying
-- @
data Bit = Bit OptVarying (Maybe ExprList)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Bit where
  toTextBuilder (Bit a b) =
    optLexemes
      [ Just "BIT",
        bool Nothing (Just "VARYING") (coerce a :: Bool),
        fmap (renderInParens . toTextBuilder) b
      ]
  parser = do
    keyword "bit"
    a <- parser
    b <- optional (Parser.space1 *> inParens parser)
    return (Bit a b)

instance Qc.Arbitrary Bit where
  shrink = Qc.genericShrink
  arbitrary = Bit <$> arbitrary <*> arbitrary
