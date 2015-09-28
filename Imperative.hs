module Imperative(Task,
                  task,
                  Coherence(..),
                  Privilege(..)) where

data Task
  = Task {
    taskName :: String,
    taskBody :: [ImperativeStmt]
    } deriving (Eq, Ord, Show)

task = Task

data ImperativeStmt
  = RuntimeCall [String]
  | IndexSpaceInit String Int Int
  | FieldSpaceInit String [String]
  | LogicalRegionInit String String String
  | TaskLaunch String [RegionRequirement]
    deriving (Eq, Ord, Show)

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
