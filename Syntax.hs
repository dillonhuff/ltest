module Syntax(tasksToTestCase,
              task,
              regionRequirement, indexSpace, fieldSpace, logicalRegion,
              Privilege(..), Coherence(..)) where

import Data.List as L

import CPPCode

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
    lrIndexSpace :: IndexSpace,
    lrFieldSpace :: FieldSpace
    } deriving (Eq, Ord, Show)

logicalRegion = LogicalRegion

data IndexSpace
  = IndexSpace Int Int
    deriving (Eq, Ord, Show)

indexSpace = IndexSpace

data FieldSpace
  = FieldSpace [String]
    deriving (Eq, Ord, Show)

fieldSpace = FieldSpace

fieldNames (FieldSpace ns) = ns

data Privilege
  = RW
  | RO
    deriving (Eq, Ord, Show)

data Coherence
  = ATOMIC
  | SIMULTANEOUS
  | EXCLUSIVE
    deriving (Eq, Ord, Show)

tasksToTestCase :: [Task] -> [CPPTopLevelItem]
tasksToTestCase ts =
  testBoilerplate ++
  [taskIDs ts, fieldIDs ts] ++
  [topLevelTask ts] ++
  taskFunctions ts ++
  [mainFunction ts]

testBoilerplate =
  [include "legion.h",
   namespace "LegionRuntime::HighLevel"]

taskIDs ts = enum "TaskIDs" $ "TOP_LEVEL_TASK_ID" : L.map (\t -> taskName t) ts

fieldIDs ts = enum "FieldIDs" ids
  where
    rrs = L.concatMap (\t -> taskRRS t) ts
    ids = L.nub $ L.concatMap (\rr -> fieldNames $ lrFieldSpace $ rrRegion rr) rrs

topLevelTask ts =
  function void "top_level_task" taskArgs []

taskFunctions ts = L.map taskFunction uniqueTasks
  where
    uniqueTasks = L.nub ts

taskFunction t =
  function void (taskName t) taskArgs []

mainFunction ts =
  function int "main" [(int, "argc"), (ptr $ ptr $ char, "argv")] (mainBody ts)

mainBody ts =
  [exprStmt $ functionCall "HighLevelRuntime::set_top_level_task_id" [] [cppVar "TOP_LEVEL_TASK_ID"],
   exprStmt $ functionCall "HighLevelRuntime::register_legion_task" [functionType "top_level_task"] [cppVar "TOP_LEVEL_TASK_ID", cppVar "Processor::LOC_PROC", cppVar "true", cppVar "false"]] ++
  taskRegistrationCode ts ++
  [returnStmt $ functionCall "HighLevelRuntime::start" [] [cppVar "argc", cppVar "argv"]]

taskRegistrationCode ts = L.map registerTaskCode uniqueTasks
  where
    uniqueTasks = L.nub ts

registerTaskCode t =
  exprStmt $ functionCall "HighLevelRuntime::register_legion_task" [functionType $ taskName t] [cppVar $ taskName t, cppVar "Processor::LOC_PROC", cppVar "true", cppVar "false"]

taskArgs =
  [(constq $ ptr $ objectType "Task", "task"),
   (constq $ ref $ templateObjectType "std::vector" [objectType "PhysicalRegion"], "regions"),
   (objectType "Context", "ctx"),
   (ptr $ objectType "HighLevelRuntime", "runtime")]
