module PostgresqlSyntax.Extras.NonEmpty where

import Data.List.NonEmpty
import PostgresqlSyntax.Prelude hiding (cons, fromList, head, init, last, reverse, tail, uncons)

-- |
-- >>> intersperseFoldMap ", " id (fromList ["a"])
-- "a"
--
-- >>> intersperseFoldMap ", " id (fromList ["a", "b", "c"])
-- "a, b, c"
intersperseFoldMap :: Monoid m => m -> (a -> m) -> NonEmpty a -> m
intersperseFoldMap a b (c :| d) = b c <> foldMap (mappend a . b) d

unsnoc :: NonEmpty a -> (Maybe (NonEmpty a), a)
unsnoc =
  let build1 = \case
        a :| b -> build2 a b
      build2 a = \case
        b : c -> build3 b (a :| []) c
        _ -> (Nothing, a)
      build3 a b = \case
        c : d -> build3 c (cons a b) d
        _ -> (Just (reverse b), a)
   in build1

consAndUnsnoc :: a -> NonEmpty a -> (NonEmpty a, a)
consAndUnsnoc a b = case unsnoc b of
  (c, d) -> case c of
    Just e -> (cons a e, d)
    Nothing -> (pure a, d)
