module TreeEnum(basicTreeCases) where

import Data.List as L
import Data.Map as M
import Control.Monad.Random

import Common
import Imperative
import PruneTreeCase
import TreeCase

basicTreeCases :: IO [TestCase]
basicTreeCases = do
  bsc <- basicCases dlr 10 3 1
  return $ L.map (\c -> compileTreeCase $ pruneTreeCase c) bsc

basicCases :: LogicalRegion -> Int -> Int -> Int -> IO [TreeCase]
basicCases r maxCases maxTasks maxRegionRequirements = do
  numCases <- getRandomR (1, maxCases)
  sequence $ L.map (\i -> randCase r i maxTasks maxRegionRequirements) [1..numCases]

randCase :: LogicalRegion -> Int -> Int -> Int -> IO TreeCase
randCase r caseNum maxTasks maxRRS = do
  rts <- randTasks r maxTasks maxRRS
  return $ treeCase ("case" ++ show caseNum) dlr dis rts

randTasks :: LogicalRegion -> Int -> Int -> IO [HighLevelTask]
randTasks r maxTasks maxRegionRequirements = do
  numTasks <- getRandomR (0, maxTasks)
  tasks <- sequence $ L.map (\i -> randTask r i maxRegionRequirements) [1..numTasks]
  case L.and $ L.map (\t -> htRRS t == []) tasks of
   True -> randTasks r maxTasks maxRegionRequirements
   False -> return tasks

randTask :: LogicalRegion -> Int -> Int -> IO HighLevelTask
randTask r taskNum maxRRS = do
  numRRS <- getRandomR (0, maxRRS)
  rrs <- sequence $ L.replicate numRRS (randRRS r)
  return $ highLevelTask ("task_" ++ show taskNum) $ L.nub rrs

randRRS :: LogicalRegion -> IO RegionRequirement
randRRS r = do
  priv <- randPrivilege
  coh <- randCoherence
  fs <- randFields r
  (reg, parent) <- randRegion r
  return $ regionRequirement reg fs priv coh parent

randRegion :: LogicalRegion -> IO (String, String)
randRegion r = return (lrName r, lrName r)

randFields :: LogicalRegion -> IO [String]
randFields r = return $ fsFields $ lrFieldSpace r

randPrivilege :: IO Privilege
randPrivilege = randElem [RO, RW]

randCoherence :: IO Coherence
randCoherence = randElem [SIMULTANEOUS, ATOMIC, EXCLUSIVE]

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
