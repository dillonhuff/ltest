module TreeEnum(basicTreeCases) where

import Data.List as L
import Data.Map as M
import Control.Monad
import Control.Monad.Random
import Control.Monad.State.Lazy

import Common
import Imperative
import PruneTreeCase
import TreeCase

data TreeGenSettings
  = TreeGenSettings {
    maxDepth :: Int,
    maxBreadth :: Int,
    maxFields :: Int,
    indexRange :: Int
    } deriving (Eq, Ord, Show)

treeDefaults = TreeGenSettings 3 4 10 29

data TaskGenSettings
  = TaskGenSettings {
    maxTasks :: Int,
    maxRegionRequirements :: Int
    } deriving (Eq, Ord, Show)

taskDefaults = TaskGenSettings 10 1

seed = 1213

basicTreeCases :: IO [TestCase]
basicTreeCases = do
  bsc <- basicCases 10 treeDefaults taskDefaults
  return $ L.map (\c -> compileTreeCase $ pruneTreeCase c) bsc

basicCases :: Int -> TreeGenSettings -> TaskGenSettings -> IO [TreeCase]
basicCases numCases treeSet taskSet =
  sequence $ L.map (\i -> randCase i (seed + i) treeSet taskSet) [1..numCases]

randCase :: Int -> Int -> TreeGenSettings -> TaskGenSettings -> IO TreeCase
randCase caseNum seed treeSet taskSet = do
  (r, i) <- randTreeData seed treeSet
  rts <- randTasks r taskSet
  return $ treeCase ("case" ++ show caseNum) r i rts

randTreeData :: Int -> TreeGenSettings -> IO (LogicalRegion, IndexSpace)
randTreeData seed treeSet =
  let indTree = randIndexTree seed treeSet in
   do
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

type RandNameState a = RandT StdGen (State NameSource) a

randIndexTree :: Int -> TreeGenSettings -> IndexSpace
randIndexTree seed treeSet =
  evalRandState seed $ randIndTree treeSet

evalRandState :: Int -> RandNameState a -> a
evalRandState seed comp =
  evalState (evalRandT comp (mkStdGen seed)) (nameSource "i" 0)

randIndTree :: TreeGenSettings -> RandNameState IndexSpace
randIndTree treeSet = do
  numParts <- getRandomR (0, maxBreadth treeSet)
  indName <- freshName
  parts <- sequence $ L.replicate numParts (randIndPart 1 treeSet)
  return $ indexSpace indName 0 (indexRange treeSet) parts

randIndPart :: Int -> TreeGenSettings -> RandNameState IndexPartition
randIndPart depth ts = do
  n <- freshName
  numChildren <- getRandomR (1, maxBreadth ts)
  subs <- sequence $ L.map (randIndSub (depth+1) ts) [0..(numChildren - 1)]
  return $ indexPartition n (allDisjoint $ L.nub subs) 0 numChildren (indMap $ L.nub subs)

indMap subs =
  M.fromList $ L.zip [0..(length subs - 1)] subs

allDisjoint subs =
  L.and [disjoint a b || a == b | a <- subs, b <- subs]

disjoint a b = (indSubStart a) > (indSubEnd b) ||
               (indSubStart b) > (indSubEnd a)

randIndSub :: Int -> TreeGenSettings -> Int -> RandNameState IndexSubspace
randIndSub depth ts i = do
  n <- freshName
  case depth >= maxDepth ts of
   True -> return $ indexSubspace n i 0 0 []
   False -> do
     numParts <- getRandomR (1, maxBreadth ts)
     parts <- sequence $ L.replicate numParts $ randIndPart (depth+1) ts
     return $ indexSubspace n i 0 0 parts

freshName :: RandNameState String
freshName = do
  n <- lift freshNameState
  return n

freshNameState :: State NameSource String
freshNameState = do
  ns <- get
  let (newNS, n) = nextName ns in
   do
     put newNS
     return n

data NameSource = NameSource String Int
                  deriving (Eq, Ord, Show)

nameSource p n = NameSource p n

nextName (NameSource p n) = (NameSource p (n+1), p ++ show n)

randTasks :: LogicalRegion -> TaskGenSettings -> IO [HighLevelTask]
randTasks r taskSet = do
  numTasks <- getRandomR (0, maxTasks taskSet)
  tasks <- sequence $ L.map (\i -> randTask r i (maxRegionRequirements taskSet)) [1..numTasks]
  case L.and $ L.map (\t -> htRRS t == []) tasks of
   True -> randTasks r taskSet
   False -> return tasks

randTask :: LogicalRegion -> Int -> Int -> IO HighLevelTask
randTask r taskNum maxRRS = do
  numRRS <- getRandomR (0, maxRRS)
  rrs <- sequence $ L.replicate numRRS (randRRS r)
  return $ highLevelTask ("task_" ++ show taskNum) $ L.nubBy (\r1 r2 -> rrRegion r1 == rrRegion r2) rrs

randRRS :: LogicalRegion -> IO RegionRequirement
randRRS r = do
  priv <- randPrivilege
  coh <- randCoherence
  fs <- randFields r
  (reg, parent) <- randRegion r
  return $ regionRequirement reg fs priv coh parent

randRegion :: LogicalRegion -> IO (String, String)
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
     randSubregionRec nextChild (lsName ls)

boolChance :: Int -> IO Bool
boolChance c = do
  v <- getRandomR (1, c)
  return $ v == 1

stopConst = 20

randFields :: LogicalRegion -> IO [String]
randFields r =
  let fieldNames = fsFields $ lrFieldSpace r in
   do
     elems <- randElems fieldNames
     return $ L.nub elems

randPrivilege :: IO Privilege
randPrivilege = randElem [RO, RW]

randCoherence :: IO Coherence
randCoherence = randElem [SIMULTANEOUS, ATOMIC, EXCLUSIVE]

randElems :: [a] -> IO [a]
randElems l = do
  numElems <- getRandomR (1, length l)
  sequence $ L.replicate numElems (randElem l)

randElem :: [a] -> IO a
randElem l = do
  ind <- getRandomR (0, length l - 1)
  return $ l !! ind

taskPairs = cartProd (tasks "a") (tasks "b")

tasks n = L.map (\rr -> highLevelTask n [rr]) rrs

rrs = L.map (\(p, c) -> regionRequirement "lr1" ["X"] p c "lr1") taskAccesses

dlr = logicalRegion "lr1" "i1" (fieldSpace "fs1" ["X"]) [lrDis]

lrDis = regionPartition "lrDis" "indPart" 0 1 $ M.fromList [(0, lrP1), (1, lrP2)]

lrP1 = logicalSubregion "lrP1" 0 []
lrP2 = logicalSubregion "lrP2" 1 []

dis = indexSpace "i1" 0 15 partitions

partitions =
  [disPart, overlapPart]

disPart =
  indexPartition "indPart" True 0 1 (M.fromList [(0, indP1), (1, indP2)])

indP1 = indexSubspace "indP1" 0 0 7 [indP1Part]
indP2 = indexSubspace "indP2" 1 8 15 []

ovP1 = indexSubspace "ovS1" 0 0 11 []
ovP2 = indexSubspace "ovS2" 1 5 14 []
ovP3 = indexSubspace "ovS3" 2 1 2 []

indP1Part =
  indexPartition "indPart1Part" False 0 1 (M.fromList [(0, indP10), (1, indP11)])

indP10 = indexSubspace "indP10" 0 1 2 []
indP11 = indexSubspace "indP11" 1 2 5 []

overlapPart =
  indexPartition "overlapPart" False 0 2 (M.fromList [(0, ovP1), (1, ovP2), (2, ovP3)])

taskAccesses = cartProd [RO, RW] [SIMULTANEOUS, ATOMIC, EXCLUSIVE]

cartProd xs ys = [(x, y) | x <- xs, y <- ys]
