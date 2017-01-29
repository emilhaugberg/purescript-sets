module Data.Set where

import Data.Array as Arr
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (maybe)
import Data.Monoid (class Monoid)
import Prelude

data Set a = Set (Array a)

-- | check if a set contains a value a
contains :: forall a. Eq a => a -> Set a -> Boolean
contains val = maybe false (const true) <<< Arr.findIndex (eq val) <<< fromSet

-- | check if set a is a subset of b
subset :: forall a. Eq a => Set a -> Set a -> Boolean
subset a b =
  foldl (\bool a' -> bool && contains a' b) true a

-- | check if set a is a proper subset of b
proper :: forall a. Eq a => Set a -> Set a -> Boolean
proper a b = subset a b && not (eq a b)

-- | check if sets are transitive
-- | transitive sets A, B, and C such that if A `f` B and B `f` C then A `f` C
transitive :: forall a. (Set a -> Set a -> Boolean) -> Set a -> Set a -> Set a -> Boolean
transitive f a b c = if a `f` b && b `f` c then true else false

fromSet :: forall a. Set a -> Array a
fromSet (Set a) = a

empty :: forall a. Set a
empty = Set []

instance eqSet :: Eq a => Eq (Set a) where
  eq (Set arr) (Set arr') = arr == arr'

instance semigroupSet :: Semigroup (Set a) where
  append (Set arr) (Set arr') = Set (append arr arr')

instance monoidSet :: Monoid (Set a) where
  mempty = empty

instance foldableSet :: Foldable Set where
  foldMap f = foldMap f <<< fromSet
  foldl f x = foldl f x <<< fromSet
  foldr f x = foldr f x <<< fromSet
