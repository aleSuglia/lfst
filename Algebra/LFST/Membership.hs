{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Membership types for the Fuzzy Set definition
module Algebra.LFST.Membership
( GodelMembership (..)
, GoguenMembership (..)
, LukasiewiczMembership (..)
  ) where

import qualified Algebra.Lattice as L

-- | Membership value between 0 and 1 with Godel join and meet operators
newtype GodelMembership = Godel Double deriving (Show, Eq, Ord, Num)

-- | Membership value between 0 and 1 with Goguen join and meet operators
newtype GoguenMembership = Goguen Double deriving (Show, Eq, Ord, Num)

-- | Membership value between 0 and 1 with Lukasiewicz join and meet operators
newtype LukasiewiczMembership = Lukas Double deriving (Show, Eq, Ord, Num)

instance L.JoinSemiLattice GodelMembership where
    Godel x \/ Godel y = Godel (max x y)

instance L.MeetSemiLattice GodelMembership where
    Godel x /\ Godel y = Godel (min x y)

instance L.Lattice GodelMembership where

instance L.BoundedJoinSemiLattice GodelMembership where
    bottom = Godel 0.0

instance L.BoundedMeetSemiLattice GodelMembership where
    top = Godel 1.0

instance L.BoundedLattice GodelMembership where

instance L.JoinSemiLattice GoguenMembership where
    Goguen x \/ Goguen y = Goguen (x + y - x * y)

instance L.MeetSemiLattice GoguenMembership where
    Goguen x /\ Goguen y = Goguen (x * y)

instance L.Lattice GoguenMembership where

instance L.BoundedJoinSemiLattice GoguenMembership where
    bottom = Goguen 0.0

instance L.BoundedMeetSemiLattice GoguenMembership where
    top = Goguen 1.0

instance L.BoundedLattice GoguenMembership where

instance L.JoinSemiLattice LukasiewiczMembership where
    Lukas x \/ Lukas y = Lukas (min 1.0 (x + y))

instance L.MeetSemiLattice LukasiewiczMembership where
    Lukas x /\ Lukas y = Lukas (max 0.0 (x + y - 1))

instance L.Lattice LukasiewiczMembership where

instance L.BoundedJoinSemiLattice LukasiewiczMembership where
    bottom = Lukas 0.0

instance L.BoundedMeetSemiLattice LukasiewiczMembership where
    top = Lukas 1.0

instance L.BoundedLattice LukasiewiczMembership where
