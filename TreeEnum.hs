module TreeEnum(basicTreeCases) where

import Data.List as L
import Data.Map as M
import Control.Monad
import Control.Monad.Random

import Common
import Imperative
import PruneTreeCase
import RandNameState
import TreeCase

data TreeGenSettings
  = TreeGenSettings {
    maxDepth :: Int,
    maxBreadth :: Int,
    maxFields :: Int,
    indexRange :: Int
    } deriving (Eq, Ord, Show)

treeDefaults = TreeGenSettings 4 3 10 29

data TaskGenSettings
  = TaskGenSettings {
    maxTasks :: Int,
    maxRegionRequirements :: Int
    } deriving (Eq, Ord, Show)

taskDefaults = TaskGenSettings 20 1

seed = 17
numCases = 10

basicTreeCases :: [TestCase]
basicTreeCases =
  L.map (\c -> compileTreeCase $ pruneTreeCase c) $ evalRandState seed (basicCases numCases treeDefaults taskDefaults)

basicCases :: Int -> TreeGenSettings -> TaskGenSettings -> RandNameState [TreeCase]
basicCases numCases treeSet taskSet =
  sequence $ L.map (\i -> randCase i treeSet taskSet) [1..numCases]

randCase :: Int -> TreeGenSettings -> TaskGenSettings -> RandNameState TreeCase
randCase caseNum treeSet taskSet = do
  (r, i) <- randTreeData treeSet
  rts <- randTasks r taskSet
  return $ treeCase ("case" ++ show caseNum) r i rts

randTreeData :: TreeGenSettings -> RandNameState (LogicalRegion, IndexSpace)
randTreeData treeSet = do
  indTree <- randIndTree treeSet
  regTree <- randLogicalRegionTree "lr" treeSet indTree
  return (regTree, indTree)

randLogicalRegionTree name treeSet indSpace = do
  numFields <- getRandomR (1, maxFields treeSet)
  return $ logicalRegion name (indName indSpace) (fieldSpace (name ++ "_fields") (L.map (\i -> "FIELD_" ++ show i) [1..numFields])) $ L.map logicalPartitionFromInd $ indParts indSpace

logicalPartitionFromInd ip =
  regionPartition ("rp" ++ ipName ip) (ipName ip) (ipColorStart ip) (ipColorEnd ip) (logicalPartMapFromIPMap $ ipChildren ip)

logicalPartMapFromIPMap ipMap =
  M.map logicalSubregionFromIndSubspace ipMap

logicalSubregionFromIndSubspace is =
  logicalSubregion ("ls" ++ indSubName is) (indSubColor is) $ L.map logicalPartitionFromInd $ indSubParts is

randIndTree :: TreeGenSettings -> RandNameState IndexSpace
randIndTree treeSet = do
  numParts <- getRandomR (0, maxBreadth treeSet)
  indName <- freshName
  parts <- sequence $ L.replicate numParts (randIndPart 0 (indexRange treeSet) 1 treeSet)
  return $ indexSpace indName 0 (indexRange treeSet) parts

randIndPart :: Int -> Int -> Int -> TreeGenSettings -> RandNameState IndexPartition
randIndPart parentStart parentEnd depth ts = do
  n <- freshName
  numChildren <- getRandomR (1, maxBreadth ts)
  subs <- sequence $ L.map (randIndSub parentStart parentEnd (depth+1) ts) [0..(numChildren - 1)]
  return $ indexPartition n (allDisjoint $ L.nub subs) 0 numChildren (indMap $ L.nub subs)

indMap subs =
  M.fromList $ L.zip [0..(length subs - 1)] subs

allDisjoint subs =
  L.and [disjoint a b || a == b | a <- subs, b <- subs]

disjoint a b = (indSubStart a) > (indSubEnd b) ||
               (indSubStart b) > (indSubEnd a)

randIndSub :: Int -> Int -> Int -> TreeGenSettings -> Int -> RandNameState IndexSubspace
randIndSub parentStart parentEnd depth ts i = do
  n <- freshName
  (start, end) <- randSubspaceBounds parentStart parentEnd
  case depth >= maxDepth ts of
   True -> return $ indexSubspace n i start end []
   False -> do
     numParts <- getRandomR (1, maxBreadth ts)
     parts <- sequence $ L.replicate numParts $ randIndPart parentStart parentEnd (depth+1) ts
     return $ indexSubspace n i start end parts

randSubspaceBounds :: Int -> Int -> RandNameState (Int, Int)
randSubspaceBounds start end = do
  randStart <- getRandomR (start, end)
  randEnd <- getRandomR (randStart, end)
  return (randStart, randEnd)

randTasks :: LogicalRegion -> TaskGenSettings -> RandNameState [HighLevelTask]
randTasks r taskSet = do
  numTasks <- getRandomR (0, maxTasks taskSet)
  tasks <- sequence $ L.map (\i -> randTask r i (maxRegionRequirements taskSet)) [1..numTasks]
  case L.and $ L.map (\t -> htRRS t == []) tasks of
   True -> randTasks r taskSet
   False -> return tasks

randTask :: LogicalRegion -> Int -> Int -> RandNameState HighLevelTask
randTask r taskNum maxRRS = do
  numRRS <- getRandomR (0, maxRRS)
  rrs <- sequence $ L.replicate numRRS (randRRS r)
  return $ highLevelTask ("task_" ++ show taskNum) $ L.nubBy (\r1 r2 -> rrRegion r1 == rrRegion r2) rrs

randRRS :: LogicalRegion -> RandNameState RegionRequirement
randRRS r = do
  priv <- randPrivilege
  coh <- randCoherence
  fs <- randFields r
  (reg, parent) <- randRegion r
  return $ regionRequirement reg fs priv coh parent

randRegion :: LogicalRegion -> RandNameState (String, String)
randRegion r = do
  shouldStop <- boolChance stopConst
  case shouldStop || lrParts r == [] of
   True -> return (lrName r, lrName r)
   False -> do
     nextPart <- randElem $ lrParts r
     nextChild <- randElem $ M.elems $ rpColorMap nextPart
     randSubregionRec nextChild (lrName r)

randSubregionRec ls parentName = do
  shouldStop <- boolChance stopConst
  case shouldStop || lsParts ls == [] of
   True -> return (lsName ls, parentName)
   False -> do
     nextPart <- randElem $ lsParts ls
     nextChild <- randElem $ M.elems $ rpColorMap nextPart
     randSubregionRec nextChild parentName

boolChance :: Int -> RandNameState Bool
boolChance c = do
  v <- getRandomR (1, c)
  return $ v == 1

stopConst = 20

randFields :: LogicalRegion -> RandNameState [String]
randFields r =
  let fieldNames = fsFields $ lrFieldSpace r in
   do
     elems <- randElems fieldNames
     return $ L.nub elems

randPrivilege :: RandNameState Privilege
randPrivilege = randElem [RO, RW]

randCoherence :: RandNameState Coherence
randCoherence = randElem [SIMULTANEOUS, ATOMIC, EXCLUSIVE]

randElems :: [a] -> RandNameState [a]
randElems l = do
  numElems <- getRandomR (1, length l)
  sequence $ L.replicate numElems (randElem l)

randElem :: [a] -> RandNameState a
randElem l = do
  ind <- getRandomR (0, length l - 1)
  return $ l !! ind
