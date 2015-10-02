module PruneTreeCase(pruneTreeCase) where

import Data.List as L
import Data.Map as M
import Data.Maybe

import Common
import TreeCase

pruneTreeCase c =
  let prunedRegions = pruneLR (ttTasks c) $ ttRegion c in
   treeCase (ttName c) prunedRegions (ttIndexSpace c) (ttTasks c)

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
  let c = rpColorMap rp
      neededLS = catMaybes $ L.map (pruneLS tasks) $ M.elems c in
   M.fromList $ L.zip [0..((L.length neededLS) - 1)] neededLS
  
neededLR tasks r =
  L.elem (lrName r) $ L.concatMap (\t -> L.map (\rr -> rrRegion rr) $ htRRS t) tasks

neededLS tasks r =
  L.elem (lsName r) $ L.concatMap (\t -> L.map (\rr -> rrRegion rr) $ htRRS t) tasks
