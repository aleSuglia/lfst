{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Membership types for the Fuzzy Set definition
module Membership
( ZadehMembership (..)
, PAMembership (..)
  ) where

import qualified Algebra.Lattice as L

-- | Membership value between 0 and 1 with min and max operators
newtype ZadehMembership = Z Double deriving (Show, Eq, Ord, Num)

-- | Membership value between 0 and 1 with algebraic sum and product operators
newtype PAMembership = PA Double deriving (Show, Eq, Ord, Num)

instance L.JoinSemiLattice ZadehMembership where
    Z x \/ Z y = Z (max x y)

instance L.MeetSemiLattice ZadehMembership where
    Z x /\ Z y = Z (min x y)

instance L.Lattice ZadehMembership where

instance L.BoundedJoinSemiLattice ZadehMembership where
    bottom = Z 0.0

instance L.BoundedMeetSemiLattice ZadehMembership where
    top = Z 1.0

instance L.BoundedLattice ZadehMembership where

instance L.JoinSemiLattice PAMembership where
    PA x \/ PA y = PA (x + y - x * y)

instance L.MeetSemiLattice PAMembership where
    PA x /\ PA y = PA (x * y)

instance L.Lattice PAMembership where

instance L.BoundedJoinSemiLattice PAMembership where
    bottom = PA 0.0

instance L.BoundedMeetSemiLattice PAMembership where
    top = PA 1.0

instance L.BoundedLattice PAMembership where
