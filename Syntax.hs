module Syntax(tasksToTestCase,
              task,
              regionRequirement, indexSpace, fieldSpace, logicalRegion,
              Privilege(..), Coherence(..)) where

import Data.Char
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

taskIDs ts = enum "TaskIDs" $ "TOP_LEVEL_TASK_ID" : L.map (\t -> L.map toUpper $ taskName t) ts

fieldIDs ts = enum "FieldIDs" ids
  where
    rrs = L.concatMap (\t -> taskRRS t) ts
    ids = L.nub $ L.concatMap (\rr -> fieldNames $ lrFieldSpace $ rrRegion rr) rrs

topLevelTask ts =
  function void "top_level_task" taskArgs (topLevelBody ts)

topLevelBody ts =
  indexSpaceCreation ts ++
  fieldSpaceCreation ts ++
  logicalRegionCreation ts ++
  taskLaunches ts ++
  cleanup ts

indexSpaceCreation ts = L.map initIndSpaceCode uniqueIndSpaces
  where
    uniqueIndSpaces = L.nubBy (\i1 i2 -> indName i1 == indName i2) indSpaces
    regionReqs = L.concatMap taskRRS ts
    indSpaces = L.map (\rr -> lrIndexSpace $ rrRegion rr) regionReqs

initIndSpaceCode ind =
  objInitStmt (objectType "IndexSpace") (indName ind) indExpr
  where
    indExpr = ptrMethodCall runtime "create_index_space" [] [ctx, dom]
    dom = functionCall "Domain::from_rect" [objectType "1"] [rect]
    rect = tempObject "Rect" [objectType "1"] [startPoint, endPoint]
    startPoint = tempObject "Point" [objectType "1"] [cppVar $ show $ indStart ind]
    endPoint = tempObject "Point" [objectType "1"] [cppVar $ show $ indEnd ind]

fieldSpaceCreation ts = L.concatMap initFieldSpaceCode uniqueFieldSpaces
  where
    uniqueFieldSpaces = L.nubBy (\f1 f2 -> fsName f1 == fsName f2) fieldSpaces
    fieldSpaces = L.map (\rr -> lrFieldSpace $ rrRegion rr) $ L.concatMap taskRRS ts

initFieldSpaceCode fs = [declSpace, fieldAllocBlock]
  where
    declSpace = objInitStmt (objectType "FieldSpace") (fsName fs) fsExpr
    fsExpr = ptrMethodCall runtime "create_field_space" [] [ctx]
    fieldAllocBlock = blockStmt $ fieldAllocStmts fs

fieldAllocStmts fs =
  fieldAllocatorInit:(fieldAllocs fs)
  where
    fieldAllocatorInit = objInitStmt (objectType "FieldAllocator") "allocator" faExpr
    faExpr = ptrMethodCall runtime "create_field_allocator" [] [ctx, cppVar $ fsName fs]

fieldAllocs fs = L.map fieldAlloc $ fieldNames fs

fieldAlloc fieldName =
  exprStmt $ refMethodCall allocator "allocate_field" [] [functionCall "sizeof" [] [cppVar "int"], cppVar fieldName]

logicalRegionCreation ts = L.map logicalRegionAllocCode uniqueLogicalRegions
  where
    uniqueLogicalRegions = L.nubBy (\lr1 lr2 -> lrName lr1 == lrName lr2) logicalRegions
    logicalRegions = L.map (\rr -> rrRegion rr) $ L.concatMap taskRRS ts

logicalRegionAllocCode lr =
  objInitStmt (objectType "LogicalRegion") (lrName lr) lrExpr
  where
    lrExpr = ptrMethodCall runtime "create_logical_region" [] [ctx, cppVar $ indName $ lrIndexSpace lr, cppVar $ fsName $ lrFieldSpace lr]

taskLaunches ts = []

cleanup ts = []

allocator = cppVar "allocator"
ctx = cppVar "ctx"
runtime = cppVar "runtime"

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
  exprStmt $ functionCall "HighLevelRuntime::register_legion_task" [functionType $ taskName t] [cppVar $ L.map toUpper $ taskName t, cppVar "Processor::LOC_PROC", cppVar "true", cppVar "false"]

taskArgs =
  [(constq $ ptr $ objectType "Task", "task"),
   (constq $ ref $ templateObjectType "std::vector" [objectType "PhysicalRegion"], "regions"),
   (objectType "Context", "ctx"),
   (ptr $ objectType "HighLevelRuntime", "runtime")]
