module PostgresqlSyntax.Ast.Typename where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.SimpleTypename
import PostgresqlSyntax.Ast.TypenameArrayDimensions
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import PostgresqlSyntax.Settings (resolveNullabilityMarkers)
import qualified Test.QuickCheck as Qc

data Typename
  = Typename
      Bool
      SimpleTypename
      Bool
      (Maybe (TypenameArrayDimensions, Bool))
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Typename where
  toTextBuilder settings (Typename setof base typeNullable arrayDims) =
    bool "" "SETOF " setof
      <> toTextBuilder settings base
      <> marker typeNullable
      <> foldMap renderArray arrayDims
    where
      markersOn = resolveNullabilityMarkers settings
      marker flag = if markersOn && flag then "?" else mempty
      renderArray (dims, flag) = toTextBuilder settings dims <> marker flag
  parser settings = do
    setof <- option False (Parsers.keyword "setof" *> Parsers.space1 $> True)
    base <- parser settings
    Parser.endHead
    let marker = if resolveNullabilityMarkers settings then Parsers.trueIfPresent (Parsers.char '?') else pure False
    typeNullable <- marker
    arrayDims <- optional $ do
      dims <- parser settings
      flag <- marker
      pure (dims, flag)
    pure (Typename setof base typeNullable arrayDims)

instance Qc.Arbitrary Typename where
  shrink = Qc.genericShrink
  arbitrary = Typename <$> arbitrary <*> arbitrary <*> pure False <*> ((\a -> (,) a False) <$$> arbitrary)
    where
      (<$$>) = fmap . fmap
