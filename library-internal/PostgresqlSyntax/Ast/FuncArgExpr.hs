module PostgresqlSyntax.Ast.FuncArgExpr where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder settings = \case
    ExprFuncArgExpr a -> toTextBuilder settings a
    ColonEqualsFuncArgExpr a b -> toTextBuilder settings a <> " := " <> toTextBuilder settings b
    EqualsGreaterFuncArgExpr a b -> toTextBuilder settings a <> " => " <> toTextBuilder settings b
  parser settings =
    asum
      [ do
          a <- Parser.wrapToHead typeFunctionName
          Parsers.space
          asum
            [ do
                Parsers.string ":="
                Parser.endHead
                b <- Parsers.space *> parser settings
                return (ColonEqualsFuncArgExpr a b),
              do
                Parsers.string "=>"
                Parser.endHead
                b <- Parsers.space *> parser settings
                return (EqualsGreaterFuncArgExpr a b)
            ],
        ExprFuncArgExpr <$> parser settings
      ]
    where
      -- Duplicated from "PostgresqlSyntax.Parsing"'s @typeFunctionName@
      -- (a bare-aliased 'PostgresqlSyntax.Ast.Ident' whose own parser lives
      -- above this module in the dependency order), mirroring the
      -- 'PostgresqlSyntax.Ast.AnyName'\/'PostgresqlSyntax.Ast.NameList'
      -- precedent.
      typeFunctionName =
        Parsers.keywordNameFromSet UnquotedIdent KeywordSet.typeFunctionName
          <|> parser settings

instance Qc.Arbitrary FuncArgExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprFuncArgExpr <$> Gens.downscale Qc.arbitrary,
        ColonEqualsFuncArgExpr <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary,
        EqualsGreaterFuncArgExpr <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary
      ]
