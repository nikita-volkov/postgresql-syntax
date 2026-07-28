module PostgresqlSyntax.Ast.TargetEl where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import {-# SOURCE #-} qualified PostgresqlSyntax.Ast.AExpr as AExpr
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
  toTextBuilder = \case
    AliasedExprTargetEl a b -> toTextBuilder a <> " AS " <> toTextBuilder b
    ImplicitlyAliasedExprTargetEl a b -> toTextBuilder a <> " " <> toTextBuilder b
    ExprTargetEl a -> toTextBuilder a
    AsteriskTargetEl -> "*"

  -- \|
  -- >>> testParser targetEl "a.b as c"
  -- AliasedExprTargetEl (CExprAExpr (ColumnrefCExpr (Columnref (UnquotedIdent "a") (Just (AttrNameIndirectionEl (UnquotedIdent "b") :| []))))) (UnquotedIdent "c")
  parser =
    Parser.label "target" $
      asum
        [ do
            expr <- parser
            asum
              [ do
                  Parser.space1
                  asum
                    [ AliasedExprTargetEl expr <$> (keyword "as" *> Parser.space1 *> Parser.endHead *> colLabel),
                      ImplicitlyAliasedExprTargetEl expr <$> parser
                    ],
                pure (ExprTargetEl expr)
              ],
          AsteriskTargetEl <$ Parser.char '*'
        ]
    where
      -- \|
      -- Duplicated from "PostgresqlSyntax.Parsing"'s @colLabel@ (a
      -- bare-aliased 'PostgresqlSyntax.Ast.Ident' whose own, more
      -- permissive parser lives above this module in the dependency
      -- order), mirroring the 'PostgresqlSyntax.Ast.AnyName' precedent.
      colLabel =
        Parser.label "column label" $
          keywordNameFromSet UnquotedIdent KeywordSet.keyword
            <|> parser

instance Qc.Arbitrary TargetEl where
  arbitrary =
    Qc.oneof
      [ pure AsteriskTargetEl,
        AliasedExprTargetEl <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary,
        -- \| Unlike 'AliasedExprTargetEl' (separated from its alias by the
        -- reserved @AS@ keyword) or 'ExprTargetEl' (followed only by a
        -- comma\/end of list, neither valid @a_expr@ continuations), the
        -- expr here is followed directly by a bare alias identifier with
        -- nothing but a space — exactly the hazard
        -- 'PostgresqlSyntax.Ast.AExpr.isBoundedAExprOperand' guards
        -- against (e.g. a trailing 'PostgresqlSyntax.Ast.AExpr.SuffixQualOpAExpr'
        -- would otherwise swallow the alias as its own operand instead).
        ImplicitlyAliasedExprTargetEl <$> AExpr.safeAExprOperand (Qc.scale (`div` 2) Qc.arbitrary) <*> Qc.arbitrary,
        ExprTargetEl <$> Qc.scale (`div` 2) Qc.arbitrary
      ]
