module TreeCase(basicTasks,
                compileTreeCase) where

import Data.List as L

import Common
import Imperative

basicTasks =
  L.map compileTreeCase [basicCase]
  
basicCase =
  treeCase "case1" (logicalRegion "lr1" (indexSpace "i1" 0 4) (fieldSpace "fs1" ["X", "Y"])) [highLevelTask "a" [], highLevelTask "b" []]

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
  task "top_level_task" []

taskStubs ts =
  L.map taskCode ts

taskCode tsk =
  task (htName tsk) []
