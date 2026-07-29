module PostgresqlSyntax.Ast.TargetEl where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import {-# SOURCE #-} qualified PostgresqlSyntax.Ast.AExpr as AExpr
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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

  -- >>> testParser targetEl "a.b as c"
  -- AliasedExprTargetEl (CExprAExpr (ColumnrefCExpr (Columnref (UnquotedIdent "a") (Just (AttrNameIndirectionEl (UnquotedIdent "b") :| []))))) (UnquotedIdent "c")
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
        -- Unlike 'AliasedExprTargetEl' (separated from its alias by the
        -- reserved @AS@ Parsers.keyword) or 'ExprTargetEl' (followed only by a
        -- comma\/end of list, neither valid @a_expr@ continuations), the
        -- expr here is followed directly by a bare alias identifier with
        -- nothing but a space — exactly the hazard
        -- 'PostgresqlSyntax.Ast.AExpr.isBoundedAExprOperand' guards
        -- against (e.g. rendering an 'PostgresqlSyntax.Ast.AExpr.OrAExpr'
        -- bare here would let its right operand absorb the alias).
        ImplicitlyAliasedExprTargetEl <$> AExpr.safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Qc.arbitrary,
        ExprTargetEl <$> Gens.downscale Qc.arbitrary
      ]
