module Syntax(Task,
              taskName, taskRRS,
              task,
              RegionRequirement,
              rrRegion, rrPrivilege, rrCoherence,
              regionRequirement,
              LogicalRegion,
              logicalRegion,
              lrName, lrFieldSpace, lrIndexSpace,
              IndexSpace,
              indexSpace,
              indName, indStart, indEnd,
              FieldSpace,
              fieldSpace,
              fsName, fieldNames,
              Privilege(..), Coherence(..)) where

data Task
  = Task {
    taskName :: String,
    taskRRS :: [RegionRequirement]
    } deriving (Eq, Ord, Show)

task = Task

data RegionRequirement
  = RegionRequirement {
    rrRegion :: LogicalRegion,
    rrPrivilege :: Privilege,
    rrCoherence :: Coherence
    } deriving (Eq, Ord, Show)

regionRequirement = RegionRequirement

data LogicalRegion
  = LogicalRegion {
    lrName :: String,
    lrIndexSpace :: IndexSpace,
    lrFieldSpace :: FieldSpace
    } deriving (Eq, Ord, Show)

logicalRegion = LogicalRegion

data IndexSpace
  = IndexSpace String Int Int
    deriving (Eq, Ord, Show)

indexSpace = IndexSpace

indName (IndexSpace n _ _) = n
indStart (IndexSpace _ s _) = s
indEnd (IndexSpace _ _ e) = e

data FieldSpace
  = FieldSpace String [String]
    deriving (Eq, Ord, Show)

fieldSpace = FieldSpace

fieldNames (FieldSpace _ ns) = ns
fsName (FieldSpace n _) = n

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
