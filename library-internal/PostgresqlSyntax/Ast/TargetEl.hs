module PostgresqlSyntax.Ast.TargetEl where

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
-- target_el:
--   |  a_expr AS ColLabel
--   |  a_expr IDENT
--   |  a_expr
--   |  '*'
-- @
data TargetEl
  = AliasedExprTargetEl AExpr Ident
  | ImplicitlyAliasedExprTargetEl AExpr Ident
  | ExprTargetEl AExpr
  | AsteriskTargetEl
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TargetEl where
  toTextBuilder settings = \case
    AliasedExprTargetEl a b -> toTextBuilder settings a <> " AS " <> toTextBuilder settings b
    ImplicitlyAliasedExprTargetEl a b -> toTextBuilder settings a <> " " <> toTextBuilder settings b
    ExprTargetEl a -> toTextBuilder settings a
    AsteriskTargetEl -> "*"

  parser settings =
    Parser.label "target" $
      asum
        [ do
            expr <- parser settings
            asum
              [ do
                  Parsers.space1
                  asum
                    [ AliasedExprTargetEl expr <$> (Parsers.keyword "as" *> Parsers.space1 *> Parser.endHead *> colLabel),
                      ImplicitlyAliasedExprTargetEl expr <$> parser settings
                    ],
                pure (ExprTargetEl expr)
              ],
          AsteriskTargetEl <$ Parsers.char '*'
        ]
    where
      -- Duplicated from "PostgresqlSyntax.Parsing"'s @colLabel@ (a
      -- bare-aliased 'PostgresqlSyntax.Ast.Ident' whose own, more
      -- permissive parser lives above this module in the dependency
      -- order), mirroring the 'PostgresqlSyntax.Ast.AnyName' precedent.
      colLabel =
        Parser.label "column label" $
          Parsers.keywordNameFromSet UnquotedIdent KeywordSet.keyword
            <|> parser settings

instance Qc.Arbitrary TargetEl where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ pure AsteriskTargetEl,
        AliasedExprTargetEl <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary,
        ImplicitlyAliasedExprTargetEl <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary,
        ExprTargetEl <$> Gens.downscale Qc.arbitrary
      ]
