{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | If X is a collection of objects denoted generically by x, then a fuzzy set F(A) in X is a set of ordered pairs.
-- Each of them consists of an element x and a membership function which maps x to the membership space M.
module FuzzySet
( FuzzySet (..)
, preimage
, empty
, add
, support
, mu
, core
, alphaCut
, fromList
, map1
, map2
, union
, intersection
, complement
, algebraicSum
, algebraicProduct
, generalizedProduct
, ExoFunctor (..)
  ) where

import Prelude hiding (fmap)
import GHC.Exts (Constraint)
import qualified Algebra.Lattice as L
import qualified Data.List       as List
import qualified Data.Map        as Map
import qualified Data.Maybe      as Maybe ()

-- $setup
-- >>> import Membership
-- >>> let godel1 = fromList [(1, Godel 0.2), (2, Godel 0.5)]
-- >>> let godel2 = fromList [(2, Godel 0.2), (3, Godel 0.2)]
-- >>> let goguen1 = fromList [(1, Goguen 0.2), (2, Goguen 0.5)]
-- >>> let goguen2 = fromList [(2, Goguen 0.2), (3, Goguen 0.2)]
-- >>> let lukas1 = fromList [(1, Lukas 0.2), (2, Lukas 0.5)]
-- >>> let lukas2 = fromList [(2, Lukas 0.2), (3, Lukas 0.2)]

-- | Returns the preimage of the given set in input
-- prop> preimage (^2) 25 [1..5] == [5]
preimage :: (Eq i, Eq j) => (i -> j) ->  j -> [i] ->  [i]
preimage f y xs = [x | x <- xs, f x == y]

-- | FuzzySet type definition
newtype FuzzySet m i = FS (Map.Map i m) deriving (Eq, Ord)

instance (Ord i, L.BoundedLattice m, Show i, Show m) => Show (FuzzySet m i) where
  show (FS fs) = "FuzzySet {" ++  List.intercalate "," [show p | p <- Map.assocs fs] ++ "}"

-- | Returns an empty fuzzy set
empty :: (Ord i, L.BoundedLattice m) => FuzzySet m i
empty = FS Map.empty

-- | Inserts a new pair (i, m) to the fuzzy set
-- prop> add godel1 (i, L.bottom) == godel1
-- prop> add goguen1 (i, L.bottom) == goguen1
-- prop> add lukas1 (i, L.bottom) == lukas1
add :: (Ord i, Eq m, L.BoundedLattice m) => FuzzySet m i -> (i, m) -> FuzzySet m i
add (FS fs) (i, m) = if m == L.bottom then FS fs else FS (Map.insert i m fs)

-- | Returns the fuzzy set's support
-- prop> support godel1 == [1, 2]
-- prop> support goguen1 == [1, 2]
-- prop> support lukas1 == [1, 2]
support :: (Ord i, L.BoundedLattice m) => FuzzySet m i -> [i]
support (FS fs) = Map.keys fs

-- | Returns the element i's membership
-- if i belongs to the support returns its membership, otherwise returns bottom lattice value
-- prop> mu godel1 1 == Godel 0.2
-- prop> mu godel1 10 == L.bottom
-- prop> mu goguen1 1 == Goguen 0.2
-- prop> mu goguen1 10 == L.bottom
-- prop> mu lukas1 1 == Lukas 0.2
-- prop> mu lukas1 10 == L.bottom
mu :: (Ord i, L.BoundedLattice m) => FuzzySet m i -> i -> m
mu (FS fs) i = case result of
  Nothing -> L.bottom
  (Just m) -> m
  where result = Map.lookup i fs

-- | Returns the crisp subset of given fuzzy set consisting of all elements with membership equals to one
-- prop> core (fromList [(-1, Godel 0.5), (0, Godel 0.8), (1, Godel 1.0), (2, Godel 0.4)]) == [1]
-- prop> core (fromList [(-1, Goguen 0.5), (0, Goguen 0.8), (1, Goguen 1.0), (2, Goguen 0.4)]) == [1]
-- prop> core (fromList [(-1, Lukas 0.5), (0, Lukas 0.8), (1, Lukas 1.0), (2, Lukas 0.4)]) == [1]
core :: (Ord i, Eq m, L.BoundedLattice m) => FuzzySet m i -> [i]
core fs = preimage (mu fs) L.top (support fs)

-- | Returns those elements whose memberships are greater or equal than the given alpha
-- prop> alphaCut (fromList [(-1, Godel 0.5), (0, Godel 0.8), (1, Godel 1.0), (2, Godel 0.4)]) (Godel 0.5) == [-1, 0, 1]
-- prop> alphaCut (fromList [(-1, Goguen 0.5), (0, Goguen 0.8), (1, Goguen 1.0), (2, Goguen 0.4)]) (Goguen 0.5) == [-1, 0, 1]
-- prop> alphaCut (fromList [(-1, Lukas 0.5), (0, Lukas 0.8), (1, Lukas 1.0), (2, Lukas 0.4)]) (Lukas 0.5) == [-1, 0, 1]
alphaCut :: (Ord i, Ord m, L.BoundedLattice m) => FuzzySet m i -> m -> [i]
alphaCut fs alpha = [i | i <- support fs, mu fs i >= alpha]

-- | Builds a fuzzy set from a list of pairs
-- prop> fromList [(1, Godel 0.2)] == add empty (1, Godel 0.2)
-- prop> fromList [(1, Goguen 0.2)] == add empty (1, Goguen 0.2)
-- prop> fromList [(1, Lukas 0.2)] == add empty (1, Lukas 0.2)
fromList :: (Ord i, Eq m, L.BoundedLattice m) => [(i, m)] -> FuzzySet m i
fromList = foldl add empty

-- | Applies a unary function to the specified fuzzy set
-- prop> map1 (*2) godel1 == fromList [(1, Godel 0.4), (2, Godel 1.0)]
-- prop> map1 (*2) goguen1 == fromList [(1, Goguen 0.4), (2, Goguen 1.0)]
-- prop> map1 (*2) lukas1 == fromList [(1, Lukas 0.4), (2, Lukas 1.0)]
map1 :: (Ord i, Eq m, L.BoundedLattice m) => (m -> m) -> FuzzySet m i -> FuzzySet m i
map1 f fs = fromList [(i, f (mu fs i)) | i <- support fs]

-- | Applies a binary function to the two specified fuzzy sets
-- prop> map2 (+) godel1 godel2 == fromList [(1, Godel 0.2), (2, Godel 0.7), (3, Godel 0.2)]
-- prop> map2 (+) goguen1 goguen2 == fromList [(1, Goguen 0.2), (2, Goguen 0.7), (3, Goguen 0.2)]
-- prop> map2 (+) lukas1 lukas2 == fromList [(1, Lukas 0.2), (2, Lukas 0.7), (3, Lukas 0.2)]
map2 :: (Ord i, Eq m, L.BoundedLattice m) => (m -> m -> m) -> FuzzySet m i -> FuzzySet m i -> FuzzySet m i
map2 f fs1 fs2 = fromList [(i, f (mu fs1 i) (mu fs2 i))| i <- union_support]
  where union_support = support fs1 `List.union` support fs2

-- | Returns the union between the two specified fuzzy sets
-- prop> union godel1 godel2 == fromList [(1, Godel 0.2), (2, Godel 0.5), (3, Godel 0.2)]
-- prop> union goguen1 goguen2 == fromList [(1, Goguen 0.2), (2, Goguen 0.6), (3, Goguen 0.2)]
-- prop> union lukas1 lukas2 == fromList [(1, Lukas 0.2), (2, Lukas 0.7), (3, Lukas 0.2)]
union :: (Ord i, Eq m, L.BoundedLattice m) => FuzzySet m i -> FuzzySet m i -> FuzzySet m i
union = map2 (L.\/)

-- | Returns the intersection between the two specified fuzzy sets
-- prop> intersection godel1 godel2 == fromList [(2, Godel 0.2)]
-- prop> intersection goguen1 goguen2 == fromList [(2, Goguen 0.1)]
-- prop> intersection lukas1 lukas2 == empty
intersection :: (Ord i, Eq m, L.BoundedLattice m) => FuzzySet m i -> FuzzySet m i -> FuzzySet m i
intersection = map2 (L./\)

-- | Returns the complement of the specified fuzzy set
-- prop> complement godel1 == fromList [(1, Godel 0.8), (2, Godel 0.5)]
-- prop> complement goguen1 == fromList [(1, Goguen 0.8), (2, Goguen 0.5)]
-- prop> complement lukas1 == fromList [(1, Lukas 0.8), (2, Lukas 0.5)]
complement :: (Ord i, Num m, Eq m, L.BoundedLattice m) => FuzzySet m i -> FuzzySet m i
complement fs = fromList [(x, L.top - mu fs x) | x <- support fs]

-- | Returns the algebraic sum between the two specified fuzzy sets
-- prop> algebraicSum godel1 godel2 == fromList [(1, Godel 0.2), (2, Godel 0.7), (3, Godel 0.2)]
-- prop> algebraicSum goguen1 goguen2 == fromList [(1, Goguen 0.2), (2, Goguen 0.7), (3, Goguen 0.2)]
-- prop> algebraicSum lukas1 lukas2 == fromList [(1, Lukas 0.2), (2, Lukas 0.7), (3, Lukas 0.2)]
algebraicSum :: (Ord i, Eq m, Num m, L.BoundedLattice m) => FuzzySet m i -> FuzzySet m i -> FuzzySet m i
algebraicSum = map2 (+)

-- | Returns the algebraic product between the two specified fuzzy sets
-- prop> algebraicProduct godel1 godel2 == fromList [(2, Godel 0.1)]
-- prop> algebraicProduct goguen1 goguen2 == fromList [(2, Goguen 0.1)]
-- prop> algebraicProduct lukas1 lukas2 == fromList [(2, Lukas 0.1)]
algebraicProduct :: (Ord i, Eq m, Num m, L.BoundedLattice m) => FuzzySet m i -> FuzzySet m i -> FuzzySet m i
algebraicProduct = map2 (*)

-- | Returns the cartesian product between two fuzzy sets using the specified function
-- prop> generalizedProduct (+) godel1 godel2 == fromList [((1, 2), Godel 0.4), ((1, 3), Godel 0.4), ((2, 2), Godel 0.7), ((2, 3), Godel 0.7)]
-- prop> generalizedProduct (+) goguen1 goguen2 == fromList [((1, 2), Goguen 0.4), ((1, 3), Goguen 0.4), ((2, 2), Goguen 0.7), ((2, 3), Goguen 0.7)]
-- prop> generalizedProduct (+) lukas1 lukas2 == fromList [((1, 2), Lukas 0.4), ((1, 3), Lukas 0.4), ((2, 2), Lukas 0.7), ((2, 3), Lukas 0.7)]
generalizedProduct :: (Ord i, Ord j, Eq m, L.BoundedLattice m) => (m -> m -> m) -> FuzzySet m i -> FuzzySet m j -> FuzzySet m (i, j)
generalizedProduct f fs1 fs2 = fromList [((x1, x2), f (mu fs1 x1) (mu fs2 x2) )| x1 <- support fs1, x2 <- support fs2]

-- | Defines a mapping between sub-categories preserving morphisms
class ExoFunctor f i where
  type SubCatConstraintI f i :: Constraint
  type SubCatConstraintI f i = ()
  type SubCatConstraintJ f j :: Constraint
  type SubCatConstraintJ f j = ()

  fmap :: (SubCatConstraintI f i, SubCatConstraintJ f j) => (i -> j) -> f i -> f j

-- | Defines a functor for the FuzzySet type which allows to implement the Extension principle
-- prop> fmap (^2) (fromList [(-1, Godel 0.5), (0, Godel 0.8), (1, Godel 1.0), (2, Godel 0.4)]) == fromList [(0, Godel 0.8), (1, Godel 1.0), (4, Godel 0.4)]
-- prop> fmap (^2) (fromList [(-1, Goguen 0.5), (0, Goguen 0.8), (1, Goguen 1.0), (2, Goguen 0.4)]) == fromList [(0, Goguen 0.8), (1, Goguen 1.0), (4, Goguen 0.4)]
-- prop> fmap (^2) (fromList [(-1, Lukas 0.5), (0, Lukas 0.8), (1, Lukas 1.0), (2, Lukas 0.4)]) == fromList [(0, Lukas 0.8), (1, Lukas 1.0), (4, Lukas 0.4)]
instance (L.BoundedLattice m, Eq m) => ExoFunctor (FuzzySet m)  i where
   type SubCatConstraintI (FuzzySet m) i  = Ord i
   type SubCatConstraintJ (FuzzySet m) j  = Ord j

   fmap f fs = fromList [(f x, mu_y (f x)) | x <- support fs]
     where mu_y y = L.joins1 [ mu fs a | a <- preimage f y (support fs)]
