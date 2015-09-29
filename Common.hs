module Common(RegionRequirement,
              regionRequirement,
              rrRegion, rrFields, rrPrivilege, rrCoherence, rrParentRegion,
              Coherence(..),
              Privilege(..)) where

data RegionRequirement
  = RegionRequirement {
    rrRegion :: String,
    rrFields :: [String],
    rrPrivilege :: Privilege,
    rrCoherence :: Coherence,
    rrParentRegion :: String
    } deriving (Eq, Ord, Show)

regionRequirement = RegionRequirement

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
