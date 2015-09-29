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

taskIDs ts = enum "TaskIDs" $ "TOP_LEVEL_TASK_ID" : L.map (\t -> L.map toUpper $ taskName t) ts

fieldIDs fs = enum "FieldIDs" fs

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

taskFunctions ts = L.map taskFunction uniqueTasks
  where
    uniqueTasks = L.nub ts

taskFunction t =
  function void (taskName t) taskArgs $ L.concatMap impStmtToCPP $ taskBody t

{-
topLevelBody ts =
  indexSpaceCreation ts ++
  fieldSpaceCreation ts ++
  logicalRegionCreation ts ++
  taskLaunches ts ++
  cleanup ts

taskLaunches ts =
  L.concatMap launchTaskCode ts

launchTaskCode t =
  [initTaskLauncher] ++ setRegionRequirements t ++ [launchTask]
  where
    initTaskLauncher = varDeclStmt (objectType "TaskLauncher") (taskName t ++ "_launcher") [cppVar $ L.map toUpper $ taskName t, tempObject "TaskArgument" [] [cppVar "NULL", cppVar "0"]]
    launchTask = exprStmt $ ptrMethodCall runtime "execute_task" [] [ctx, cppVar $ (taskName t) ++ "_launcher"]

setRegionRequirements t = L.concat $ L.zipWith (buildRRCode (taskName t ++ "_launcher")) [0..(length (taskRRS t) - 1)] (taskRRS t)

buildRRCode launcherName n rr = (addRR launcherName rr):(addFields launcherName n rr)

addFields launcherName n rr = L.map (addFieldCode (cppVar launcherName) n) fieldVars
  where
    fieldVars = L.map cppVar $ rrFields rr

addFieldCode launcherNameVar n fVar =
  exprStmt $ refMethodCall launcherRRN "add_field" [] [fVar]
  where
    launcherRRN = arrayRef launcherNameVar "region_requirements" (cppVar $ show n)

addRR launcherName rr =
  exprStmt $ refMethodCall (cppVar launcherName) "add_region_requirement" [] [rrObj]
  where
    rrObj = tempObject "RegionRequirement" [] [rName, privilege, coherence, rName]
    rName = cppVar $ lrName $ rrRegion rr
    privilege = cppVar $ show $ rrPrivilege rr
    coherence = cppVar $ show $ rrCoherence rr

-}
