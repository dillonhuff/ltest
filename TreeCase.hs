module TreeCase(treeCase,
                highLevelTask,
                LogicalRegion,
                logicalRegion,
                indexSpace,
                fieldSpace,
                compileTreeCase) where

import Data.List as L

import Common
import Imperative

data TreeCase
  = TreeCase {
    ttName :: String,
    ttRegion :: LogicalRegion,
    ttTasks :: [HighLevelTask]
    } deriving (Eq, Ord, Show)

treeCase = TreeCase

data HighLevelTask
  = HighLevelTask {
    htName :: String,
    htRRS :: [RegionRequirement]
    } deriving (Eq, Ord, Show)

highLevelTask = HighLevelTask

data LogicalRegion
  = LogicalRegion {
    lrName :: String,
    lrIndexSpace :: IndexSpace,
    lrFieldSpace :: FieldSpace
    } deriving (Eq, Ord, Show)

logicalRegion = LogicalRegion

data IndexSpace
  = IndexSpace {
    indName :: String,
    indStart :: Int,
    indEnd :: Int
    } deriving (Eq, Ord, Show)

indexSpace = IndexSpace

data FieldSpace
  = FieldSpace {
    fsName :: String,
    fsFields :: [String]
    } deriving (Eq, Ord, Show)

fieldSpace = FieldSpace

compileTreeCase t =
  testCase (ttName t) (fsFields $ lrFieldSpace $ ttRegion t) (compileTasks t)

compileTasks t =
  (topLevelTask t):(taskStubs $ ttTasks t)

topLevelTask t =
  task "top_level_task" (topLevelTaskBody t)

topLevelTaskBody t =
  dataInit (ttRegion t) ++
  taskLaunches (ttTasks t) ++
  cleanup (ttRegion t)

dataInit r =
  [indexSpaceInit (indName is) (indStart is) (indEnd is),
   fieldSpaceInit (fsName fs) (fsFields fs),
   logicalRegionInit (lrName r) (indName is) (fsName fs)]
  where
    is = lrIndexSpace r
    fs = lrFieldSpace r

taskLaunches tsks =
  L.map (\tsk -> taskLaunch (htName tsk) (htRRS tsk)) tsks

cleanup r =
  [runtimeCall "destroy_index_space" ["ctx", indName $ lrIndexSpace r],
   runtimeCall "destroy_field_space" ["ctx", fsName $ lrFieldSpace r],
   runtimeCall "destroy_logical_region" ["ctx", lrName r]]

taskStubs ts =
  L.map taskCode ts

taskCode tsk =
  task (htName tsk) []
