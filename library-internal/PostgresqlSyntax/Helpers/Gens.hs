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

-- | Generate a non-empty list of at most @n + 1@ elements, splitting the size
-- budget across them.
--
-- The split is what keeps growth bounded: generating every element at the
-- undiminished size would multiply the subtree's cost by the list length at no
-- size cost, and those multipliers compound through the AST.
nonEmptyUpTo :: Int -> Gen a -> Gen (NonEmpty a)
nonEmptyUpTo n gen = sized $ \size -> do
  -- The 'max 0' matters: at size 0 the upper bound is negative, and 'choose'
  -- silently swaps inverted bounds instead of failing.
  tailLen <- choose (0, max 0 (min n (size - 1)))
  let totalLen = tailLen + 1
      subsize = size `div` totalLen
      subgen = resize subsize gen
  x <- subgen
  xs <- vectorOf tailLen subgen
  pure (x :| xs)

terminatingMaybe :: Gen a -> Gen (Maybe a)
terminatingMaybe gen = sized $ \size ->
  if size <= 1
    then pure Nothing
    else Just <$> gen
