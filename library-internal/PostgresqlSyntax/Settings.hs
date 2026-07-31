-- |
-- Parse\/render options for the 'PostgresqlSyntax.Algebra' machinery.
--
-- The type is abstract: build a 'Settings' value with 'nullabilityMarkers' and
-- combine values with their 'Semigroup'\/'Monoid' instances ('mempty' is
-- faithful, standard Postgres). Because no constructor or field selector is
-- exported, a resolved configuration indirection can be introduced later
-- without a breaking change.
module PostgresqlSyntax.Settings
  ( Settings,
    nullabilityMarkers,
    resolveNullabilityMarkers,
  )
where

import PostgresqlSyntax.Prelude

-- |
-- \@Settings {optNullabilityMarkers = Nothing}\@ everywhere means standard
-- Postgres. The only knob today is whether the 'PostgresqlSyntax.Ast.Typename'
-- @?@ nullability markers are recognized.
data Settings = Settings {optNullabilityMarkers :: Maybe Bool}
  deriving (Show, Eq)

-- | Per option the right operand wins; an unset ('Nothing') option falls back
-- to the left operand's value.
instance Semigroup Settings where
  a <> b = Settings {optNullabilityMarkers = optNullabilityMarkers b <|> optNullabilityMarkers a}

instance Monoid Settings where
  mempty = Settings Nothing

-- |
-- The only public constructor: opt into (or out of) the
-- 'PostgresqlSyntax.Ast.Typename' @?@ nullability markers.
nullabilityMarkers :: Bool -> Settings
nullabilityMarkers = Settings . Just

-- |
-- Resolve the nullability-marker option to its effective value, defaulting to
-- @False@ (standard Postgres). Internal — used at parse\/render sites, not
-- re-exported from the "PostgresqlSyntax" facade.
resolveNullabilityMarkers :: Settings -> Bool
resolveNullabilityMarkers = fromMaybe False . optNullabilityMarkers
