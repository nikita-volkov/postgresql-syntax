module PostgresqlSyntax.Ast.FuncExprCommonSubexpr where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.ExtractList
import PostgresqlSyntax.Ast.OverlayList
import PostgresqlSyntax.Ast.PositionList
import PostgresqlSyntax.Ast.SubstrList
import PostgresqlSyntax.Ast.TrimList
import PostgresqlSyntax.Ast.TrimModifier
import PostgresqlSyntax.Ast.Typename
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- func_expr_common_subexpr:
--   | COLLATION FOR '(' a_expr ')'
--   | CURRENT_DATE
--   | CURRENT_TIME
--   | CURRENT_TIME '(' Iconst ')'
--   | CURRENT_TIMESTAMP
--   | CURRENT_TIMESTAMP '(' Iconst ')'
--   | LOCALTIME
--   | LOCALTIME '(' Iconst ')'
--   | LOCALTIMESTAMP
--   | LOCALTIMESTAMP '(' Iconst ')'
--   | CURRENT_ROLE
--   | CURRENT_USER
--   | SESSION_USER
--   | USER
--   | CURRENT_CATALOG
--   | CURRENT_SCHEMA
--   | CAST '(' a_expr AS Typename ')'
--   | EXTRACT '(' extract_list ')'
--   | OVERLAY '(' overlay_list ')'
--   | POSITION '(' position_list ')'
--   | SUBSTRING '(' substr_list ')'
--   | TREAT '(' a_expr AS Typename ')'
--   | TRIM '(' BOTH trim_list ')'
--   | TRIM '(' LEADING trim_list ')'
--   | TRIM '(' TRAILING trim_list ')'
--   | TRIM '(' trim_list ')'
--   | NULLIF '(' a_expr ',' a_expr ')'
--   | COALESCE '(' expr_list ')'
--   | GREATEST '(' expr_list ')'
--   | LEAST '(' expr_list ')'
--   | XMLCONCAT '(' expr_list ')'
--   | XMLELEMENT '(' NAME_P ColLabel ')'
--   | XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ')'
--   | XMLELEMENT '(' NAME_P ColLabel ',' expr_list ')'
--   | XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ',' expr_list ')'
--   | XMLEXISTS '(' c_expr xmlexists_argument ')'
--   | XMLFOREST '(' xml_attribute_list ')'
--   | XMLPARSE '(' document_or_content a_expr xml_whitespace_option ')'
--   | XMLPI '(' NAME_P ColLabel ')'
--   | XMLPI '(' NAME_P ColLabel ',' a_expr ')'
--   | XMLROOT '(' a_expr ',' xml_root_version opt_xml_root_standalone ')'
--   | XMLSERIALIZE '(' document_or_content a_expr AS SimpleTypename ')'
--
-- TODO: Implement the XML cases
-- @
data FuncExprCommonSubexpr
  = CollationForFuncExprCommonSubexpr AExpr
  | CurrentDateFuncExprCommonSubexpr
  | CurrentTimeFuncExprCommonSubexpr (Maybe Int64)
  | CurrentTimestampFuncExprCommonSubexpr (Maybe Int64)
  | LocalTimeFuncExprCommonSubexpr (Maybe Int64)
  | LocalTimestampFuncExprCommonSubexpr (Maybe Int64)
  | CurrentRoleFuncExprCommonSubexpr
  | CurrentUserFuncExprCommonSubexpr
  | SessionUserFuncExprCommonSubexpr
  | UserFuncExprCommonSubexpr
  | CurrentCatalogFuncExprCommonSubexpr
  | CurrentSchemaFuncExprCommonSubexpr
  | CastFuncExprCommonSubexpr AExpr Typename
  | ExtractFuncExprCommonSubexpr (Maybe ExtractList)
  | OverlayFuncExprCommonSubexpr OverlayList
  | PositionFuncExprCommonSubexpr (Maybe PositionList)
  | SubstringFuncExprCommonSubexpr (Maybe SubstrList)
  | TreatFuncExprCommonSubexpr AExpr Typename
  | TrimFuncExprCommonSubexpr (Maybe TrimModifier) TrimList
  | NullIfFuncExprCommonSubexpr AExpr AExpr
  | CoalesceFuncExprCommonSubexpr ExprList
  | GreatestFuncExprCommonSubexpr ExprList
  | LeastFuncExprCommonSubexpr ExprList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncExprCommonSubexpr where
  toTextBuilder = \case
    CollationForFuncExprCommonSubexpr a -> "COLLATION FOR (" <> toTextBuilder a <> ")"
    CurrentDateFuncExprCommonSubexpr -> "CURRENT_DATE"
    CurrentTimeFuncExprCommonSubexpr a -> "CURRENT_TIME" <> TextBuilders.suffixMaybe (TextBuilders.renderInParens . TextBuilder.int64Dec) a
    CurrentTimestampFuncExprCommonSubexpr a -> "CURRENT_TIMESTAMP" <> TextBuilders.suffixMaybe (TextBuilders.renderInParens . TextBuilder.int64Dec) a
    LocalTimeFuncExprCommonSubexpr a -> "LOCALTIME" <> TextBuilders.suffixMaybe (TextBuilders.renderInParens . TextBuilder.int64Dec) a
    LocalTimestampFuncExprCommonSubexpr a -> "LOCALTIMESTAMP" <> TextBuilders.suffixMaybe (TextBuilders.renderInParens . TextBuilder.int64Dec) a
    CurrentRoleFuncExprCommonSubexpr -> "CURRENT_ROLE"
    CurrentUserFuncExprCommonSubexpr -> "CURRENT_USER"
    SessionUserFuncExprCommonSubexpr -> "SESSION_USER"
    UserFuncExprCommonSubexpr -> "USER"
    CurrentCatalogFuncExprCommonSubexpr -> "CURRENT_CATALOG"
    CurrentSchemaFuncExprCommonSubexpr -> "CURRENT_SCHEMA"
    CastFuncExprCommonSubexpr a b -> "CAST (" <> toTextBuilder a <> " AS " <> toTextBuilder b <> ")"
    ExtractFuncExprCommonSubexpr a -> "EXTRACT (" <> foldMap toTextBuilder a <> ")"
    OverlayFuncExprCommonSubexpr a -> "OVERLAY (" <> toTextBuilder a <> ")"
    PositionFuncExprCommonSubexpr a -> "POSITION (" <> foldMap toTextBuilder a <> ")"
    SubstringFuncExprCommonSubexpr a -> "SUBSTRING (" <> foldMap toTextBuilder a <> ")"
    TreatFuncExprCommonSubexpr a b -> "TREAT (" <> toTextBuilder a <> " AS " <> toTextBuilder b <> ")"
    TrimFuncExprCommonSubexpr a b -> "TRIM (" <> TextBuilders.prefixMaybe toTextBuilder a <> toTextBuilder b <> ")"
    NullIfFuncExprCommonSubexpr a b -> "NULLIF (" <> toTextBuilder a <> ", " <> toTextBuilder b <> ")"
    CoalesceFuncExprCommonSubexpr a -> "COALESCE (" <> toTextBuilder a <> ")"
    GreatestFuncExprCommonSubexpr a -> "GREATEST (" <> toTextBuilder a <> ")"
    LeastFuncExprCommonSubexpr a -> "LEAST (" <> toTextBuilder a <> ")"
  parser =
    asum
      [ CollationForFuncExprCommonSubexpr <$> Parsers.inParensWithClause (Parsers.keyphrase "collation for") parser,
        CurrentDateFuncExprCommonSubexpr <$ Parsers.keyword "current_date",
        CurrentTimestampFuncExprCommonSubexpr <$> labeledIconst "current_timestamp",
        CurrentTimeFuncExprCommonSubexpr <$> labeledIconst "current_time",
        LocalTimestampFuncExprCommonSubexpr <$> labeledIconst "localtimestamp",
        LocalTimeFuncExprCommonSubexpr <$> labeledIconst "localtime",
        CurrentRoleFuncExprCommonSubexpr <$ Parsers.keyword "current_role",
        CurrentUserFuncExprCommonSubexpr <$ Parsers.keyword "current_user",
        SessionUserFuncExprCommonSubexpr <$ Parsers.keyword "session_user",
        UserFuncExprCommonSubexpr <$ Parsers.keyword "user",
        CurrentCatalogFuncExprCommonSubexpr <$ Parsers.keyword "current_catalog",
        CurrentSchemaFuncExprCommonSubexpr <$ Parsers.keyword "current_schema",
        Parsers.inParensWithClause (Parsers.keyword "cast") (CastFuncExprCommonSubexpr <$> parser <*> (Parsers.space1 *> Parsers.keyword "as" *> Parsers.space1 *> parser)),
        Parsers.inParensWithClause (Parsers.keyword "extract") (ExtractFuncExprCommonSubexpr <$> optional parser),
        Parsers.inParensWithClause (Parsers.keyword "overlay") (OverlayFuncExprCommonSubexpr <$> parser),
        Parsers.inParensWithClause (Parsers.keyword "position") (PositionFuncExprCommonSubexpr <$> optional parser),
        Parsers.inParensWithClause (Parsers.keyword "substring") (SubstringFuncExprCommonSubexpr <$> optional parser),
        Parsers.inParensWithClause (Parsers.keyword "treat") (TreatFuncExprCommonSubexpr <$> parser <*> (Parsers.space1 *> Parsers.keyword "as" *> Parsers.space1 *> parser)),
        Parsers.inParensWithClause (Parsers.keyword "trim") (TrimFuncExprCommonSubexpr <$> optional (parser <* Parsers.space1) <*> parser),
        Parsers.inParensWithClause (Parsers.keyword "nullif") (NullIfFuncExprCommonSubexpr <$> parser <*> (Parsers.commaSeparator *> parser)),
        Parsers.inParensWithClause (Parsers.keyword "coalesce") (CoalesceFuncExprCommonSubexpr <$> parser),
        Parsers.inParensWithClause (Parsers.keyword "greatest") (GreatestFuncExprCommonSubexpr <$> parser),
        Parsers.inParensWithClause (Parsers.keyword "least") (LeastFuncExprCommonSubexpr <$> parser)
      ]
    where
      labeledIconst lbl = Parsers.keyword lbl *> Parser.endHead *> optional (Parsers.space *> Parsers.inParens Parsers.decimal)

instance Qc.Arbitrary FuncExprCommonSubexpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then
          Qc.oneof
            [ pure CurrentDateFuncExprCommonSubexpr,
              pure CurrentRoleFuncExprCommonSubexpr,
              pure CurrentUserFuncExprCommonSubexpr,
              pure SessionUserFuncExprCommonSubexpr,
              pure UserFuncExprCommonSubexpr,
              pure CurrentCatalogFuncExprCommonSubexpr,
              pure CurrentSchemaFuncExprCommonSubexpr
            ]
        else
          Qc.oneof
            [ CollationForFuncExprCommonSubexpr <$> Qc.arbitrary,
              pure CurrentDateFuncExprCommonSubexpr,
              -- The @Iconst@ here is parsed via 'Parsers.decimal' (unsigned), so
              -- it must never be negative — mirroring
              -- 'PostgresqlSyntax.Ast.IntervalSecond'\'s own @nonNegative@.
              CurrentTimeFuncExprCommonSubexpr <$> nonNegativeMaybeInt64,
              CurrentTimestampFuncExprCommonSubexpr <$> nonNegativeMaybeInt64,
              LocalTimeFuncExprCommonSubexpr <$> nonNegativeMaybeInt64,
              LocalTimestampFuncExprCommonSubexpr <$> nonNegativeMaybeInt64,
              pure CurrentRoleFuncExprCommonSubexpr,
              pure CurrentUserFuncExprCommonSubexpr,
              pure SessionUserFuncExprCommonSubexpr,
              pure UserFuncExprCommonSubexpr,
              pure CurrentCatalogFuncExprCommonSubexpr,
              pure CurrentSchemaFuncExprCommonSubexpr,
              CastFuncExprCommonSubexpr <$> Qc.arbitrary <*> Qc.arbitrary,
              ExtractFuncExprCommonSubexpr <$> Qc.arbitrary,
              OverlayFuncExprCommonSubexpr <$> Qc.arbitrary,
              PositionFuncExprCommonSubexpr <$> Qc.arbitrary,
              SubstringFuncExprCommonSubexpr <$> Qc.arbitrary,
              TreatFuncExprCommonSubexpr <$> Qc.arbitrary <*> Qc.arbitrary,
              TrimFuncExprCommonSubexpr <$> Qc.arbitrary <*> Qc.arbitrary,
              NullIfFuncExprCommonSubexpr <$> Qc.arbitrary <*> Qc.arbitrary,
              CoalesceFuncExprCommonSubexpr <$> Qc.arbitrary,
              GreatestFuncExprCommonSubexpr <$> Qc.arbitrary,
              LeastFuncExprCommonSubexpr <$> Qc.arbitrary
            ]
    where
      nonNegativeMaybeInt64 = Qc.oneof [pure Nothing, Just <$> nonNegativeInt64]
      nonNegativeInt64 = Qc.sized (\n -> Qc.choose (0, cap n))
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n
