module TreeCase(treeCase,
                highLevelTask,
                LogicalRegion,
                logicalRegion,
                indexSpace,
                IndexPartition,
                indexPartition,
                IndexSubspace,
                indexSubspace,
                fieldSpace,
                compileTreeCase) where

import Data.List as L
import Data.Map as M

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
    indEnd :: Int,
    indParts :: [IndexPartition]
    } deriving (Eq, Ord, Show)

indexSpace = IndexSpace

data IndexPartition
  = IndexPartition {
    ipName :: String,
    ipIsDisjoint :: Bool,
    ipColorStart :: Int,
    ipColorEnd :: Int,
    ipChildren :: Map Int IndexSubspace
    } deriving (Eq, Ord, Show)

indexPartition = IndexPartition

data IndexSubspace
  = IndexSubspace {
    indSubName :: String,
    indSubColor :: Int,
    indSubStart :: Int,
    indSubEnd :: Int,
    indSubParts :: [IndexPartition]
    } deriving (Eq, Ord, Show)

indexSubspace = IndexSubspace

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
  indexTreeInit is ++
  [fieldSpaceInit (fsName fs) (fsFields fs),
   logicalRegionInit (lrName r) (indName is) (fsName fs)]
  where
    is = lrIndexSpace r
    fs = lrFieldSpace r

indexTreeInit is =
  [indexSpaceInit (indName is) (indStart is) (indEnd is)] ++
  (L.concatMap (indexPartitionInitCode $ indName is) $ indParts is)

indexPartitionInitCode indSpaceName p =
  (indexPartitionInit (ipName p) indSpaceName (ipIsDisjoint p) (M.map (\is -> (indSubStart is, indSubEnd is)) (ipChildren p))):
  (L.concatMap (indexSubspaceInitCode (ipName p)) $ M.elems $ ipChildren p)

indexSubspaceInitCode partName indSub =
  (indexSubspaceInit (indSubName indSub) partName (indSubColor indSub)):
  (L.concatMap (indexPartitionInitCode (indSubName indSub)) (indSubParts indSub))

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
