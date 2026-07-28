module PostgresqlSyntax.Ast.FuncArgExpr where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder = \case
    ExprFuncArgExpr a -> toTextBuilder a
    ColonEqualsFuncArgExpr a b -> toTextBuilder a <> " := " <> toTextBuilder b
    EqualsGreaterFuncArgExpr a b -> toTextBuilder a <> " => " <> toTextBuilder b
  parser =
    asum
      [ do
          a <- Parser.wrapToHead typeFunctionName
          Parser.space
          asum
            [ do
                Parser.string ":="
                Parser.endHead
                b <- Parser.space *> parser
                return (ColonEqualsFuncArgExpr a b),
              do
                Parser.string "=>"
                Parser.endHead
                b <- Parser.space *> parser
                return (EqualsGreaterFuncArgExpr a b)
            ],
        ExprFuncArgExpr <$> parser
      ]
    where
      -- \|
      -- Duplicated from "PostgresqlSyntax.Parsing"'s @typeFunctionName@
      -- (a bare-aliased 'PostgresqlSyntax.Ast.Ident' whose own parser lives
      -- above this module in the dependency order), mirroring the
      -- 'PostgresqlSyntax.Ast.AnyName'\/'PostgresqlSyntax.Ast.NameList'
      -- precedent.
      typeFunctionName =
        keywordNameFromSet UnquotedIdent KeywordSet.typeFunctionName
          <|> parser

instance Qc.Arbitrary FuncArgExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprFuncArgExpr <$> Qc.scale (`div` 2) Qc.arbitrary,
        ColonEqualsFuncArgExpr <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        EqualsGreaterFuncArgExpr <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary
      ]
