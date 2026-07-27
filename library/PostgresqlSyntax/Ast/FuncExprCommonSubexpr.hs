module PostgresqlSyntax.Ast.FuncExprCommonSubexpr where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.ExtractList
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.OverlayList
import PostgresqlSyntax.Ast.PositionList
import PostgresqlSyntax.Ast.SubstrList
import PostgresqlSyntax.Ast.Typename
import PostgresqlSyntax.Ast.TrimList
import PostgresqlSyntax.Ast.TrimModifier
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
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
    CurrentTimeFuncExprCommonSubexpr a -> "CURRENT_TIME" <> suffixMaybe (renderInParens . TextBuilder.int64Dec) a
    CurrentTimestampFuncExprCommonSubexpr a -> "CURRENT_TIMESTAMP" <> suffixMaybe (renderInParens . TextBuilder.int64Dec) a
    LocalTimeFuncExprCommonSubexpr a -> "LOCALTIME" <> suffixMaybe (renderInParens . TextBuilder.int64Dec) a
    LocalTimestampFuncExprCommonSubexpr a -> "LOCALTIMESTAMP" <> suffixMaybe (renderInParens . TextBuilder.int64Dec) a
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
    TrimFuncExprCommonSubexpr a b -> "TRIM (" <> prefixMaybe toTextBuilder a <> toTextBuilder b <> ")"
    NullIfFuncExprCommonSubexpr a b -> "NULLIF (" <> toTextBuilder a <> ", " <> toTextBuilder b <> ")"
    CoalesceFuncExprCommonSubexpr a -> "COALESCE (" <> toTextBuilder a <> ")"
    GreatestFuncExprCommonSubexpr a -> "GREATEST (" <> toTextBuilder a <> ")"
    LeastFuncExprCommonSubexpr a -> "LEAST (" <> toTextBuilder a <> ")"
  parser =
    asum
      [ CollationForFuncExprCommonSubexpr <$> inParensWithClause (keyphrase "collation for") parser,
        CurrentDateFuncExprCommonSubexpr <$ keyword "current_date",
        CurrentTimestampFuncExprCommonSubexpr <$> labeledIconst "current_timestamp",
        CurrentTimeFuncExprCommonSubexpr <$> labeledIconst "current_time",
        LocalTimestampFuncExprCommonSubexpr <$> labeledIconst "localtimestamp",
        LocalTimeFuncExprCommonSubexpr <$> labeledIconst "localtime",
        CurrentRoleFuncExprCommonSubexpr <$ keyword "current_role",
        CurrentUserFuncExprCommonSubexpr <$ keyword "current_user",
        SessionUserFuncExprCommonSubexpr <$ keyword "session_user",
        UserFuncExprCommonSubexpr <$ keyword "user",
        CurrentCatalogFuncExprCommonSubexpr <$ keyword "current_catalog",
        CurrentSchemaFuncExprCommonSubexpr <$ keyword "current_schema",
        inParensWithClause (keyword "cast") (CastFuncExprCommonSubexpr <$> parser <*> (Parser.space1 *> keyword "as" *> Parser.space1 *> parser)),
        inParensWithClause (keyword "extract") (ExtractFuncExprCommonSubexpr <$> optional parser),
        inParensWithClause (keyword "overlay") (OverlayFuncExprCommonSubexpr <$> parser),
        inParensWithClause (keyword "position") (PositionFuncExprCommonSubexpr <$> optional parser),
        inParensWithClause (keyword "substring") (SubstringFuncExprCommonSubexpr <$> optional parser),
        inParensWithClause (keyword "treat") (TreatFuncExprCommonSubexpr <$> parser <*> (Parser.space1 *> keyword "as" *> Parser.space1 *> parser)),
        inParensWithClause (keyword "trim") (TrimFuncExprCommonSubexpr <$> optional (parser <* Parser.space1) <*> parser),
        inParensWithClause (keyword "nullif") (NullIfFuncExprCommonSubexpr <$> parser <*> (commaSeparator *> parser)),
        inParensWithClause (keyword "coalesce") (CoalesceFuncExprCommonSubexpr <$> parser),
        inParensWithClause (keyword "greatest") (GreatestFuncExprCommonSubexpr <$> parser),
        inParensWithClause (keyword "least") (LeastFuncExprCommonSubexpr <$> parser)
      ]
    where
      labeledIconst lbl = keyword lbl *> Parser.endHead *> optional (Parser.space *> inParens Parser.decimal)

instance Qc.Arbitrary FuncExprCommonSubexpr where
  arbitrary =
    Qc.oneof
      [ CollationForFuncExprCommonSubexpr <$> Qc.scale (`div` 2) Qc.arbitrary,
        pure CurrentDateFuncExprCommonSubexpr,
        -- | The @Iconst@ here is parsed via 'Parser.decimal' (unsigned), so
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
        CastFuncExprCommonSubexpr <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary,
        ExtractFuncExprCommonSubexpr <$> Qc.scale (`div` 2) Qc.arbitrary,
        OverlayFuncExprCommonSubexpr <$> Qc.scale (`div` 2) Qc.arbitrary,
        PositionFuncExprCommonSubexpr <$> Qc.scale (`div` 2) Qc.arbitrary,
        SubstringFuncExprCommonSubexpr <$> Qc.scale (`div` 2) Qc.arbitrary,
        TreatFuncExprCommonSubexpr <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary,
        TrimFuncExprCommonSubexpr <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        NullIfFuncExprCommonSubexpr <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        CoalesceFuncExprCommonSubexpr <$> Qc.scale (`div` 2) Qc.arbitrary,
        GreatestFuncExprCommonSubexpr <$> Qc.scale (`div` 2) Qc.arbitrary,
        LeastFuncExprCommonSubexpr <$> Qc.scale (`div` 2) Qc.arbitrary
      ]
    where
      nonNegativeMaybeInt64 = Qc.oneof [pure Nothing, Just <$> nonNegativeInt64]
      nonNegativeInt64 = Qc.sized (\n -> Qc.choose (0, cap n))
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n
