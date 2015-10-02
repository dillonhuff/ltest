module PruneTreeCase(pruneTreeCase) where

import Data.List as L
import Data.Map as M
import Data.Maybe

import Common
import TreeCase

pruneTreeCase c =
  let (prunedRegions, prunedIS) = pruneLR (ttTasks c) (ttRegion c) (ttIndexSpace c) in
   treeCase (ttName c) prunedRegions prunedIS (ttTasks c)

pruneLR tasks r ind =
  case pruneLRRec tasks r ind of
   Just (prunedRegion, ind) -> (prunedRegion, ind)
   Nothing -> error $ "pruneLR: region " ++ show r ++ " is not needed"

pruneLRRec tasks r ind =
  case neededLR tasks r || neededPartitions /= [] of
   True -> Just $ (lrSetPartitions r $ L.map fst neededPartitions, ind)
   False -> Nothing
  where
    parts = matchLRPartitions (lrParts r) ind
    neededPartitions = catMaybes $ L.map (\(rp, ip) -> pruneRP tasks rp ip) parts

matchLRPartitions :: [RegionPartition] ->
                     IndexSpace ->
                     [(RegionPartition, IndexPartition)]
matchLRPartitions lr is = error "matchLRPartitions"

pruneLS :: [HighLevelTask] -> LogicalSubregion -> Maybe LogicalSubregion
pruneLS tasks r =
  case neededLS tasks r || neededPartitions /= [] of
   True -> Just $ lsSetPartitions r neededPartitions
   False -> Nothing
  where
    neededPartitions = error "neededPartitions" --catMaybes $ L.map (pruneRP tasks) $ lsParts r

pruneRP :: [HighLevelTask] -> RegionPartition -> IndexPartition -> Maybe (RegionPartition, IndexPartition)
pruneRP tasks rp ip =
  let (neededRegionChildren, neededIndexChildren) = rpNeededChildren tasks rp ip
      s = 0
      e = L.length neededRegionChildren
      regionChildMap = M.fromList $ L.zip [0..((L.length neededRegionChildren) - 1)] neededRegionChildren
      indexChildMap = M.fromList $ L.zip [0..((L.length neededIndexChildren) - 1)] neededIndexChildren in
   case neededRegionChildren == [] of
    True -> Nothing
    False -> Just $ (regionPartition (rpName rp) (rpIndexPartition rp) s e regionChildMap, indexPartition (ipName ip) (ipIsDisjoint ip) s e indexChildMap)

rpNeededChildren :: [HighLevelTask] -> RegionPartition -> IndexPartition -> ([LogicalSubregion], [IndexSubspace])
rpNeededChildren tasks rp ip = error "rpNeededChildren"

{-  let c = rpColorMap rp
      neededLS = catMaybes $ L.map (pruneLS tasks) $ M.elems c in
   M.fromList $ L.zip [0..((L.length neededLS) - 1)] neededLS-}
  
neededLR tasks r =
  L.elem (lrName r) $ L.concatMap (\t -> L.map (\rr -> rrRegion rr) $ htRRS t) tasks

neededLS tasks r =
  L.elem (lsName r) $ L.concatMap (\t -> L.map (\rr -> rrRegion rr) $ htRRS t) tasks
