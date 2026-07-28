module PostgresqlSyntax.Helpers.Gens where

import PostgresqlSyntax.Prelude
import Test.QuickCheck

downscale :: Gen a -> Gen a
downscale = scale (`div` 2)

recursive :: Gen a -> Gen a -> Gen a
recursive nonRecursiveGen recursiveGen = sized $ \size ->
  if size <= 1
    then nonRecursiveGen
    else downscale recursiveGen

oneofRec ::
  (Arbitrary a) =>
  [Gen a] ->
  [Gen a] ->
  Gen a
oneofRec nonRecursiveGens recursiveGens = sized $ \size ->
  if size <= 1
    then oneof nonRecursiveGens
    else
      frequency
        [ (1, oneof nonRecursiveGens),
          (3, downscale (oneof recursiveGens))
        ]

nonEmptyUpTo :: Int -> Gen a -> Gen (NonEmpty a)
nonEmptyUpTo n gen = sized $ \size -> do
  tailLen <- choose (0, min n (size - 1))
  x <- gen
  xs <- vectorOf tailLen gen
  pure (x :| xs)

terminatingMaybe :: Gen a -> Gen (Maybe a)
terminatingMaybe gen = sized $ \size ->
  if size <= 1
    then pure Nothing
    else Just <$> gen
