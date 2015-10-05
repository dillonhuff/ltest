module TestElaboration(toCPP) where

import Data.Char
import Data.List as L

import CPPCode
import Imperative

toCPP :: TestCase -> [CPPTopLevelItem]
toCPP ts =
  testBoilerplate ++
  [taskIDs $ testTasks ts, fieldIDs $ testFields ts] ++
  (taskFunctions $ testTasks ts) ++
  [mainFunction $ testTasks ts]

testBoilerplate =
  [include "legion.h",
   namespace "LegionRuntime::HighLevel"]

taskIDs ts = enum "TaskIDs" $ L.map (\t -> L.map toUpper $ taskName t) ts

fieldIDs fs = enum "FieldIDs" fs

mainFunction ts =
  function int "main" [(int, "argc"), (ptr $ ptr $ char, "argv")] (mainBody ts)

mainBody ts =
  [exprStmt $ functionCall "HighLevelRuntime::set_top_level_task_id" [] [cppVar "TOP_LEVEL_TASK"]] ++
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

taskFunctions ts = L.map taskFunction uniqueTasks
  where
    uniqueTasks = L.nub ts

taskFunction t =
  function void (taskName t) taskArgs $ L.concatMap impStmtToCPP $ taskBody t
