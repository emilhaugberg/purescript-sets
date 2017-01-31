module Data.Set where

import Data.Array as Arr
import Data.Foldable (class Foldable, foldMap, foldl, foldr, elem)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap, over)
import Prelude (class Eq, class Ord, class Semigroup, append, notEq, flip, not, (&&), (<<<), (==))

type Collection = Array

newtype Set a = Set (Array a)
derive instance newtypeSet :: Newtype (Set a) _

infixl 6 contains as ∈

-- | check if set A contains element a
contains :: forall a. Eq a => a -> Set a -> Boolean
contains = elem

infixl 6 not' as ∉

not' :: forall a. Eq a => a -> Set a -> Boolean
not' = not <<< elem

infixl 6 containsFlipped as ∋

containsFlipped :: forall a. Eq a => Set a -> a -> Boolean
containsFlipped = flip contains

-- | check if set A is a subset of set B
infixl 8 subset as ⊆

subset :: forall a. Eq a => Set a -> Set a -> Boolean
subset a b =
  foldl (\bool a' -> bool && a' ∈ b) true a

-- | check if set A is a proper subset of set B
infixl 8 proper as ⊂
infix  8 notEq  as ≠

proper :: forall a. (Eq a, Ord a) => Set a -> Set a -> Boolean
proper a b = a ⊆ b && a ≠ b

-- | Union of a collection of sets is the set of all elements in the collection
infixl 6 union as ∪

union :: forall a. Set a -> Set a -> Set a
union = append

unionCollection :: forall a. Collection (Set a) -> Set a
unionCollection = foldl (∪) empty

-- | the intersection A ∩ B of two sets A and B is the set that contains
-- | all elements of A that also belong to B (or the other way around).
infixl 8 intersection as ∩

intersection :: forall a. Eq a => Set a -> Set a -> Set a
intersection setA setB = over Set (containsElem (∈) setB) setA

infixl 8 difference as \

difference :: forall a. Eq a => Set a -> Set a -> Set a
difference setA setB = over Set (containsElem (∉) setB) setA

containsElem :: forall a. (a -> Set a -> Boolean) -> Set a -> (Array a -> Array a)
containsElem f set = Arr.filter (\x -> f x set)

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
