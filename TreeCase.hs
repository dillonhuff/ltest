module TreeCase(TreeCase,
                treeCase,
                ttName, ttTasks, ttRegion, ttIndexSpace,
                HighLevelTask,
                highLevelTask,
                htName, htRRS,
                LogicalRegion,
                logicalRegion,
                lrName, lrParts, lrSetPartitions, lrFieldSpace,
                LogicalSubregion,
                logicalSubregion,
                lsName, lsParts, lsSetPartitions, lsSetColor,
                regionPartition,
                indexSpace,
                RegionPartition,
                rpName, rpColorMap, rpIndexPartition,
                IndexSpace,
                indSetPartitions, indParts,
                IndexPartition,
                indexPartition,
                ipName, ipIsDisjoint, ipChildren,
                IndexSubspace,
                indexSubspace,
                indSubParts, indSubSetPartitions, indSubSetColor,
                FieldSpace,
                fieldSpace,
                fsName, fsFields,
                compileTreeCase) where

import Data.List as L
import Data.Map as M

import Common
import Imperative

data TreeCase
  = TreeCase {
    ttName :: String,
    ttRegion :: LogicalRegion,
    ttIndexSpace :: IndexSpace,
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
    lrIndexSpace :: String,
    lrFieldSpace :: FieldSpace,
    lrParts :: [RegionPartition]
    } deriving (Eq, Ord, Show)

logicalRegion = LogicalRegion

lrSetPartitions p (LogicalRegion n i f _) =
  logicalRegion n i f p

data LogicalSubregion
  = LogicalSubregion {
    lsName :: String,
    lsColor :: Int,
    lsParts :: [RegionPartition]
    } deriving (Eq, Ord, Show)

logicalSubregion = LogicalSubregion

lsSetPartitions p (LogicalSubregion n c _) =
  logicalSubregion n c p
lsSetColor c (LogicalSubregion n _ p) = logicalSubregion n c p

data RegionPartition
  = RegionPartition {
    rpName :: String,
    rpIndexPartition :: String,
    rpColorStart :: Int,
    rpColorEnd :: Int,
    rpColorMap :: Map Int LogicalSubregion
    } deriving (Eq, Ord, Show)

regionPartition = RegionPartition

data IndexSpace
  = IndexSpace {
    indName :: String,
    indStart :: Int,
    indEnd :: Int,
    indParts :: [IndexPartition]
    } deriving (Eq, Ord, Show)

indexSpace = IndexSpace

indSetPartitions p (IndexSpace n s e _) =
  IndexSpace n s e p

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

indSubSetPartitions p (IndexSubspace n c s e _) =
  IndexSubspace n c s e p
indSubSetColor c (IndexSubspace n _ s e p) =
  IndexSubspace n c s e p

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
  dataInit t ++
  taskLaunches (ttTasks t) ++
  cleanup t

dataInit t =
  indexTreeInit is ++
  [fieldSpaceInit (fsName fs) (fsFields fs)] ++
  logicalRegionTreeInit r
  where
    r = ttRegion t
    is = ttIndexSpace t
    fs = lrFieldSpace r

logicalRegionTreeInit r =
  (logicalRegionInit(lrName r) (lrIndexSpace r) (fsName $ lrFieldSpace r)):
  (L.concatMap (regionPartitionInitCode $ lrName r) $ lrParts r)

regionPartitionInitCode parentName part =
  (regionPartitionInit (rpName part) parentName (rpIndexPartition part)):
  (L.concatMap (regionSubspaceInitCode (rpName part)) $ M.elems $ rpColorMap part)

regionSubspaceInitCode partName subregion =
  (regionSubspaceInit (lsName subregion) partName (lsColor subregion)):[]

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

cleanup t =
  [runtimeCall "destroy_index_space" ["ctx", indName $ ttIndexSpace $ t],
   runtimeCall "destroy_field_space" ["ctx", fsName $ lrFieldSpace $ ttRegion t],
   runtimeCall "destroy_logical_region" ["ctx", lrName $ ttRegion t]]

taskStubs ts =
  L.map taskCode ts

taskCode tsk =
  task (htName tsk) []
