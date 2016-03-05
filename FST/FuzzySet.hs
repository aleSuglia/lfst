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
-- >>> let zfs1 = fromList [(1, Z 0.2), (2, Z 0.5)]
-- >>> let zfs2 = fromList [(2, Z 0.2), (3, Z 0.2)]
-- >>> let pfs1 = fromList [(1, PA 0.2), (2, PA 0.5)]
-- >>> let pfs2 = fromList [(2, PA 0.2), (3, PA 0.2)]

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
-- prop> add zfs1 (i, L.bottom) == zfs1
-- prop> add pfs1 (i, L.bottom) == pfs1
add :: (Ord i, Eq m, L.BoundedLattice m) => FuzzySet m i -> (i, m) -> FuzzySet m i
add (FS fs) (i, m) = if m == L.bottom then FS fs else FS (Map.insert i m fs)

-- | Returns the fuzzy set's support
-- prop> support zfs1 == [1, 2]
-- prop> support pfs1 == [1, 2]
support :: (Ord i, L.BoundedLattice m) => FuzzySet m i -> [i]
support (FS fs) = Map.keys fs

-- | Returns the element i's membership
-- if i belongs to the support returns its membership, otherwise returns bottom lattice value
-- prop> mu zfs1 1 == Z 0.2
-- prop> mu zfs1 10 == L.bottom
-- prop> mu pfs1 1 == PA 0.2
-- prop> mu pfs1 10 == L.bottom
mu :: (Ord i, L.BoundedLattice m) => FuzzySet m i -> i -> m
mu (FS fs) i = case result of
  Nothing -> L.bottom
  (Just m) -> m
  where result = Map.lookup i fs

-- | Returns the crisp subset of given fuzzy set consisting of all elements with membership equals to one
-- prop> core (fromList [(-1, Z 0.5), (0, Z 0.8), (1, Z 1.0), (2, Z 0.4)]) == [1]
-- prop> core (fromList [(-1, PA 0.5), (0, PA 0.8), (1, PA 1.0), (2, PA 0.4)]) == [1]
core :: (Ord i, Eq m, L.BoundedLattice m) => FuzzySet m i -> [i]
core fs = preimage (mu fs) L.top (support fs)

-- | Returns those elements whose memberships are greater or equal than the given alpha
-- prop> alphaCut (fromList [(-1, Z 0.5), (0, Z 0.8), (1, Z 1.0), (2, Z 0.4)]) (Z 0.5) == [-1, 0, 1]
-- prop> alphaCut (fromList [(-1, PA 0.5), (0, PA 0.8), (1, PA 1.0), (2, PA 0.4)]) (PA 0.5) == [-1, 0, 1]
alphaCut :: (Ord i, Ord m, L.BoundedLattice m) => FuzzySet m i -> m -> [i]
alphaCut fs alpha = [i | i <- support fs, mu fs i >= alpha]

-- | Builds a fuzzy set from a list of pairs
-- prop> fromList [(1, Z 0.2)] == add empty (1, Z 0.2)
-- prop> fromList [(1, PA 0.2)] == add empty (1, PA 0.2)
fromList :: (Ord i, Eq m, L.BoundedLattice m) => [(i, m)] -> FuzzySet m i
fromList = foldl add empty

-- | Applies a unary function to the specified fuzzy set
-- prop> map1 (*2) zfs1 == fromList [(1, Z 0.4), (2, Z 1.0)]
-- prop> map1 (*2) pfs1 == fromList [(1, PA 0.4), (2, PA 1.0)]
map1 :: (Ord i, Eq m, L.BoundedLattice m) => (m -> m) -> FuzzySet m i -> FuzzySet m i
map1 f fs = fromList [(i, f (mu fs i)) | i <- support fs]

-- | Applies a binary function to the two specified fuzzy sets
-- prop> map2 (+) zfs1 zfs2 == fromList [(1, Z 0.2), (2, Z 0.7), (3, Z 0.2)]
-- prop> map2 (+) pfs1 pfs2 == fromList [(1, PA 0.2), (2, PA 0.7), (3, PA 0.2)]
map2 :: (Ord i, Eq m, L.BoundedLattice m) => (m -> m -> m) -> FuzzySet m i -> FuzzySet m i -> FuzzySet m i
map2 f fs1 fs2 = fromList [(i, f (mu fs1 i) (mu fs2 i))| i <- union_support]
  where union_support = support fs1 `List.union` support fs2

-- | Returns the union between the two specified fuzzy sets
-- prop> union zfs1 zfs2 == fromList [(1, Z 0.2), (2, Z 0.5), (3, Z 0.2)]
-- prop> union pfs1 pfs2 == fromList [(1,PA 0.2),(2,PA 0.6),(3,PA 0.2)]
union :: (Ord i, Eq m, L.BoundedLattice m) => FuzzySet m i -> FuzzySet m i -> FuzzySet m i
union = map2 (L.\/)

-- | Returns the intersection between the two specified fuzzy sets
-- prop> intersection zfs1 zfs2 == fromList [(2, Z 0.2)]
-- prop> intersection pfs1 pfs2 == fromList [(2, PA 0.1)]
intersection :: (Ord i, Eq m, L.BoundedLattice m) => FuzzySet m i -> FuzzySet m i -> FuzzySet m i
intersection = map2 (L./\)

-- | Returns the complement of the specified fuzzy set
-- prop> complement zfs1 == fromList [(1, Z 0.8), (2, Z 0.5)]
-- prop> complement pfs1 == fromList [(1, PA 0.8), (2, PA 0.5)]
complement :: (Ord i, Num m, Eq m, L.BoundedLattice m) => FuzzySet m i -> FuzzySet m i
complement fs = fromList [(x, L.top - mu fs x) | x <- support fs]

-- | Returns the algebraic sum between the two specified fuzzy sets
-- prop> algebraicSum zfs1 zfs2 == fromList [(1, Z 0.2), (2, Z 0.7), (3, Z 0.2)]
-- prop> algebraicSum pfs1 pfs2 == fromList [(1, PA 0.2), (2, PA 0.7), (3, PA 0.2)]
algebraicSum :: (Ord i, Eq m, Num m, L.BoundedLattice m) => FuzzySet m i -> FuzzySet m i -> FuzzySet m i
algebraicSum = map2 (+)

-- | Returns the algebraic product between the two specified fuzzy sets
-- prop> algebraicProduct zfs1 zfs2 == fromList [(2, Z 0.1)]
-- prop> algebraicProduct pfs1 pfs2 == fromList [(2, PA 0.1)]
algebraicProduct :: (Ord i, Eq m, Num m, L.BoundedLattice m) => FuzzySet m i -> FuzzySet m i -> FuzzySet m i
algebraicProduct = map2 (*)

-- | Returns the cartesian product between two fuzzy sets using the specified function
-- prop> generalizedProduct (+) zfs1 zfs2 == fromList [((1, 2), Z 0.4), ((1, 3), Z 0.4), ((2, 2), Z 0.7), ((2, 3), Z 0.7)]
-- prop> generalizedProduct (+) pfs1 pfs2 == fromList [((1, 2), PA 0.4), ((1, 3), PA 0.4), ((2, 2), PA 0.7), ((2, 3), PA 0.7)]
generalizedProduct :: (Ord i, Ord j, Eq m, L.BoundedLattice m) => (m -> m -> m) -> FuzzySet m i -> FuzzySet m j -> FuzzySet m (i, j)
generalizedProduct f fs1 fs2 = fromList [((x1, x2), f (mu fs1 x1) (mu fs2 x2) )| x1 <- support fs1, x2 <- support fs2]

-- | Defines a mapping between sub-categories preserving morphisms
class ExoFunctor f i where
  type SubCatConstraintI f i :: Constraint
  type SubCatConstraintI f i = ()
  type SubCatConstraintJ f j :: Constraint
  type SubCatConstraintJ f j = ()

  fmap :: (SubCatConstraintI f i, SubCatConstraintJ f j) => (i -> j) -> f i -> f j

-- | Defines a functor for the FuzzySet type that allows to implement the Extension principle
-- prop> fmap (^2) (fromList [(-1, Z 0.5), (0, Z 0.8), (1, Z 1.0), (2, Z 0.4)]) == fromList [(0, Z 0.8), (1, Z 1.0), (4, Z 0.4)]
-- prop> fmap (^2) (fromList [(-1, PA 0.5), (0, PA 0.8), (1, PA 1.0), (2, PA 0.4)]) == fromList [(0, PA 0.8), (1, PA 1.0), (4, PA 0.4)]
instance (L.BoundedLattice m, Eq m) => ExoFunctor (FuzzySet m)  i where
   type SubCatConstraintI (FuzzySet m) i  = Ord i
   type SubCatConstraintJ (FuzzySet m) j  = Ord j

   fmap f fs = fromList [(f x, mu_y (f x)) | x <- support fs]
     where mu_y y = L.joins1 [ mu fs a | a <- preimage f y (support fs)]
