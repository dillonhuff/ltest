module PruneTreeCase(pruneTreeCase) where

import Data.List as L
import Data.Map as M
import Data.Maybe

import Common
import TreeCase

pruneTreeCase c =
  let (prunedRegions, prunedIS) = prune (ttTasks c) (ttRegion c) (ttIndexSpace c) in
   treeCase (ttName c) prunedRegions prunedIS (ttTasks c)

prune :: [HighLevelTask] -> LogicalRegion -> IndexSpace -> (LogicalRegion, IndexSpace)
prune tasks r ind =
  case pruneRec tasks r ind of
   Just (prunedRegion, ind) -> (prunedRegion, ind)
   Nothing -> error $ "pruneLR: region " ++ show r ++ " is not needed"

pruneRec :: [HighLevelTask] ->
            LogicalRegion ->
            IndexSpace ->
            Maybe (LogicalRegion, IndexSpace)
pruneRec tasks r ind =
  case neededLR tasks r || neededPartitions /= [] of
   True -> Just $ (newR, newInd)
   False -> Nothing
  where
    parts = matchLRPartitions (lrParts r) $ indParts ind
    neededPartitions = catMaybes $ L.map (\(rp, ip) -> pruneRP tasks rp ip) parts
    newR = lrSetPartitions (L.map fst neededPartitions) r
    newInd = indSetPartitions (L.map snd neededPartitions) ind

matchLRPartitions :: [RegionPartition] ->
                     [IndexPartition] ->
                     [(RegionPartition, IndexPartition)]
matchLRPartitions rParts iParts =
  L.map (\rp -> (rp, fLookup rp iParts)) rParts
  where
    fLookup i l = fromJust $ find (\ip -> ipName ip == rpIndexPartition i) l

pruneLS :: [HighLevelTask] ->
           LogicalSubregion ->
           IndexSubspace ->
           Maybe (LogicalSubregion, IndexSubspace)
pruneLS tasks r ind =
  case neededLS tasks r || neededPartitions /= [] of
   True -> Just $ (newR, newInd)
   False -> Nothing
  where
    parts = matchLRPartitions (lsParts r) $ indSubParts ind
    neededPartitions = catMaybes $ L.map (\(rp, ip) -> pruneRP tasks rp ip) parts
    newR = lsSetPartitions (L.map fst neededPartitions) r
    newInd = indSubSetPartitions (L.map snd neededPartitions) ind

  {-  case neededLS tasks r || neededPartitions /= [] of
   True -> Just $ lsSetPartitions r neededPartitions
   False -> Nothing
  where
    parts = matchLRPartitions (lsParts r) ind
    neededPartitions = catMaybes $ L.map (\(rp, ip) -> pruneRP tasks) parts-}

pruneRP :: [HighLevelTask] -> RegionPartition -> IndexPartition -> Maybe (RegionPartition, IndexPartition)
pruneRP tasks rp ip =
  let (regionChildMap, indexChildMap) = rpNeededChildren tasks rp ip
      s = 0
      e = L.maximum $ M.keys regionChildMap
      newRPart = regionPartition (rpName rp) (rpIndexPartition rp) s e regionChildMap
      newIndPart = indexPartition (ipName ip) (ipIsDisjoint ip) s e indexChildMap in
   case regionChildMap == M.empty of
    True -> Nothing
    False -> Just (newRPart, newIndPart)

    -- (regionPartition (rpName rp) (rpIndexPartition rp) s e regionChildMap, indexPartition (ipName ip) (ipIsDisjoint ip) s e indexChildMap)-}

rpNeededChildren :: [HighLevelTask] ->
                    RegionPartition ->
                    IndexPartition ->
                    (Map Int LogicalSubregion, Map Int IndexSubspace)
rpNeededChildren tasks rp ip =
  let c = rpColorMap rp
      i = ipChildren ip
      matchingChildren = matchChildrenByColor c i
      neededChildren = catMaybes $ L.map (\(sr, sp) -> pruneLS tasks sr sp) matchingChildren
      (neededSubregions, neededSubspaces) = unzip neededChildren
      subRegList = L.map (\(n, ls) -> (n, lsSetColor n ls)) $ L.zip [0..(length neededSubregions - 1)] neededSubregions
      subRegMap = M.fromList $ subRegList
      subSpaceList = L.map (\(n, is) -> (n, indSubSetColor n is)) $ L.zip [0..(length neededSubspaces - 1)] neededSubspaces
      subSpaceMap = M.fromList $ subSpaceList in
   (subRegMap, subSpaceMap)

matchChildrenByColor :: Map Int LogicalSubregion ->
                        Map Int IndexSubspace ->
                        [(LogicalSubregion, IndexSubspace)]
matchChildrenByColor rColorMap iColorMap =
  L.map (\color -> (fLookup color rColorMap, fLookup color iColorMap)) colors
  where
    colors = M.keys rColorMap
    fLookup c m = fromJust $ M.lookup c m

{-      regionChildMap = M.fromList $ L.zip [0..((L.length neededRegionChildren) - 1)] neededRegionChildren
      indexChildMap = M.fromList $ L.zip [0..((L.length neededIndexChildren) - 1)] neededIndexChildren in
-}
{-  let c = rpColorMap rp
      neededLS = catMaybes $ L.map (pruneLS tasks) $ M.elems c in
   M.fromList $ L.zip [0..((L.length neededLS) - 1)] neededLS-}
  
neededLR tasks r =
  L.elem (lrName r) $ L.concatMap (\t -> L.map (\rr -> rrRegion rr) $ htRRS t) tasks

neededLS tasks r =
  L.elem (lsName r) $ L.concatMap (\t -> L.map (\rr -> rrRegion rr) $ htRRS t) tasks
