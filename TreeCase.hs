module TreeCase(treeCase,
                highLevelTask,
                LogicalRegion,
                logicalRegion,
                LogicalSubregion,
                logicalSubregion,
                regionPartition,
                indexSpace,
                IndexPartition,
                indexPartition,
                IndexSubspace,
                indexSubspace,
                fieldSpace,
                pruneTreeCase,
                compileTreeCase) where

import Data.List as L
import Data.Map as M
import Data.Maybe

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

lrSetPartitions (LogicalRegion n i f _) p =
  logicalRegion n i f p

data LogicalSubregion
  = LogicalSubregion {
    lsName :: String,
    lsColor :: Int,
    lsParts :: [RegionPartition]
    } deriving (Eq, Ord, Show)

logicalSubregion = LogicalSubregion

lsSetPartitions (LogicalSubregion n c _) p =
  logicalSubregion n c p

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

pruneTreeCase c =
  treeCase (ttName c) (pruneLR (ttTasks c) $ ttRegion c) (ttIndexSpace c) (ttTasks c)

pruneLR tasks r =
  case pruneLRRec tasks r of
   Just prunedRegion -> prunedRegion
   Nothing -> error $ "pruneLR: region " ++ show r ++ " is not needed"

pruneLRRec tasks r =
  case neededLR tasks r || neededPartitions /= [] of
   True -> Just $ lrSetPartitions r neededPartitions
   False -> Nothing
  where
    neededPartitions = catMaybes $ L.map (pruneRP tasks) $ lrParts r

pruneLS :: [HighLevelTask] -> LogicalSubregion -> Maybe LogicalSubregion
pruneLS tasks r =
  case neededLS tasks r || neededPartitions /= [] of
   True -> Just $ lsSetPartitions r neededPartitions
   False -> Nothing
  where
    neededPartitions = catMaybes $ L.map (pruneRP tasks) $ lsParts r

pruneRP :: [HighLevelTask] -> RegionPartition -> Maybe RegionPartition
pruneRP tasks rp =
  let neededChildren = rpNeededChildren tasks rp
      s = L.minimum $ M.keys neededChildren
      e = L.maximum $ M.keys neededChildren in
   case neededChildren == M.empty of
    True -> Nothing
    False -> Just $ regionPartition (rpName rp) (rpIndexPartition rp) s e neededChildren

rpNeededChildren tasks rp =
  let c = rpColorMap rp in
   M.filter (neededLS tasks) c
  
neededLR tasks r =
  L.elem (lrName r) $ L.concatMap (\t -> L.map (\rr -> rrRegion rr) $ htRRS t) tasks

neededLS tasks r =
  L.elem (lsName r) $ L.concatMap (\t -> L.map (\rr -> rrRegion rr) $ htRRS t) tasks

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
