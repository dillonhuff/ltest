module Common(RegionRequirement,
              Coherence(..),
              Privilege(..)) where

data RegionRequirement
  = RegionRequirement {
    rrRegion :: String,
    rrPrivilege :: Privilege,
    rrCoherence :: Coherence,
    rrParentRegion :: String
    } deriving (Eq, Ord, Show)

data Privilege
  = RW
  | RO
    deriving (Eq, Ord)

instance Show Privilege where
  show RW = "READ_WRITE"
  show RO = "READ_ONLY"

data Coherence
  = ATOMIC
  | SIMULTANEOUS
  | EXCLUSIVE
    deriving (Eq, Ord, Show)
