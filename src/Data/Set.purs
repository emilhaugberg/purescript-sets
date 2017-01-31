module Data.Set where

import Data.Array as Arr
import Data.Foldable (class Foldable, foldMap, foldl, foldr, elem)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap, over)
import Prelude (class Eq, class Ord, class Semigroup, append, notEq, flip, ($), (&&), (<<<), (==))

type Collection = Array

newtype Set a = Set (Array a)
derive instance newtypeSet :: Newtype (Set a) _

infixl 6 contains as ∈

-- | check if a set contains a value a
contains :: forall a. Eq a => a -> Set a -> Boolean
contains = elem

infixr 6 containsFlipped as ∋

containsFlipped :: forall a. Eq a => Set a -> a -> Boolean
containsFlipped = flip contains

infixl 8 subset as ⊆

-- | check if set a is a subset of b
subset :: forall a. Eq a => Set a -> Set a -> Boolean
subset a b =
  foldl (\bool a' -> bool && a' ∈ b) true a

infix  8 notEq  as ≠
infixl 8 proper as ⊂

-- | check if set a is a proper subset of b
proper :: forall a. (Eq a, Ord a) => Set a -> Set a -> Boolean
proper a b = a ⊆ b && a ≠ b

-- | check if sets are transitive
-- | transitive sets A, B, and C such that if A `f` B and B `f` C then A `f` C
transitive :: forall a. (Set a -> Set a -> Boolean) -> Set a -> Set a -> Set a -> Boolean
transitive f a b c = a `f` b && b `f` c

infixl 6 union as ∪

-- | Union of a collection of sets is the set of all elements in the collection
union :: forall a. Set a -> Set a -> Set a
union = append

unionCollection :: forall a. Collection (Set a) -> Set a
unionCollection = foldl (∪) empty

infixl 8 intersection as ∩

-- | the intersection A ∩ B of two sets A and B is the set that contains
-- | all elements of A that also belong to B (or the other way around).
intersection :: forall a. Eq a => Set a -> Set a -> Set a
intersection setA setB = over Set (Arr.filter $ (∋) setB) setA

disjoint :: forall a. (Ord a, Eq a) => Set a -> Set a -> Boolean
disjoint setA setB = empty == setA ∩ setB

insert :: forall a. a -> Set a -> Set a
insert = over Set <<< Arr.cons

empty :: forall a. Set a
empty = Set []

instance eqSet :: (Eq a, Ord a) => Eq (Set a) where
  eq (Set a) (Set b) = f a == f b
    where
      f = Arr.sort <<< Arr.nub

instance semigroupSet :: Semigroup (Set a) where
  append (Set arr) (Set arr') = Set (arr `append` arr')

instance monoidSet :: Monoid (Set a) where
  mempty = empty

instance foldableSet :: Foldable Set where
  foldMap f = foldMap f <<< unwrap
  foldl f x = foldl f x <<< unwrap
  foldr f x = foldr f x <<< unwrap
