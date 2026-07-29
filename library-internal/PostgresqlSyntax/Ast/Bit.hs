module PostgresqlSyntax.Ast.Bit where

import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.OptVarying
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
  toTextBuilder settings (Bit a b) =
    TextBuilders.optLexemes
      [ Just "BIT",
        bool Nothing (Just "VARYING") (coerce a :: Bool),
        fmap (TextBuilders.renderInParens . toTextBuilder settings) b
      ]
  parser settings = do
    Parsers.keyword "bit"
    a <- parser settings
    b <- optional (Parsers.space1 *> Parsers.inParens (parser settings))
    return (Bit a b)

instance Qc.Arbitrary Bit where
  shrink = Qc.genericShrink
  arbitrary = Bit <$> arbitrary <*> arbitrary
