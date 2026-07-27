module PostgresqlSyntax.Ast.FuncArgExpr where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- func_arg_expr:
--   | a_expr
--   | param_name COLON_EQUALS a_expr
--   | param_name EQUALS_GREATER a_expr
-- param_name:
--   | type_function_name
-- @
data FuncArgExpr
  = ExprFuncArgExpr AExpr
  | ColonEqualsFuncArgExpr Ident AExpr
  | EqualsGreaterFuncArgExpr Ident AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncArgExpr where
  toTextBuilder = \case
    ExprFuncArgExpr a -> toTextBuilder a
    ColonEqualsFuncArgExpr a b -> toTextBuilder a <> " := " <> toTextBuilder b
    EqualsGreaterFuncArgExpr a b -> toTextBuilder a <> " => " <> toTextBuilder b
  parser =
    asum
      [ do
          a <- wrapToHead typeFunctionName
          space
          asum
            [ do
                string ":="
                endHead
                b <- space *> parser
                return (ColonEqualsFuncArgExpr a b),
              do
                string "=>"
                endHead
                b <- space *> parser
                return (EqualsGreaterFuncArgExpr a b)
            ],
        ExprFuncArgExpr <$> parser
      ]
    where
      -- |
      -- Duplicated from "PostgresqlSyntax.Parsing"'s @typeFunctionName@
      -- (a bare-aliased 'PostgresqlSyntax.Ast.Ident' whose own parser lives
      -- above this module in the dependency order), mirroring the
      -- 'PostgresqlSyntax.Ast.AnyName'\/'PostgresqlSyntax.Ast.NameList'
      -- precedent.
      typeFunctionName =
        keywordNameFromSet UnquotedIdent KeywordSet.typeFunctionName
          <|> parser

instance Arbitrary FuncArgExpr where
  arbitrary =
    oneof
      [ ExprFuncArgExpr <$> scale (`div` 2) arbitrary,
        ColonEqualsFuncArgExpr <$> arbitrary <*> scale (`div` 2) arbitrary,
        EqualsGreaterFuncArgExpr <$> arbitrary <*> scale (`div` 2) arbitrary
      ]
