module TreeEnum(basicTreeCases) where

import Data.List as L
import Data.Map as M

import Common
import TreeCase

-- For now shortened to make testing easier
basicTreeCases =
  [L.head $ L.map compileTreeCase basicCases]
  
basicCases = L.zipWith (\n (a, b) -> treeCase ("case" ++ show n) dlr [a, b]) [1..(length taskPairs)] taskPairs

taskPairs = cartProd (tasks "a") (tasks "b")

tasks n = L.map (\rr -> highLevelTask n [rr]) rrs

rrs = L.map (\(p, c) -> regionRequirement "lr1" ["X"] p c "lr1") taskAccesses

dlr = logicalRegion "lr1" dis (fieldSpace "fs1" ["X"])

dis = indexSpace "i1" 0 15 partitions

partitions =
  [disPart, overlapPart]

disPart =
  indexPartition "indPart" True 0 1 (M.fromList [(0, indP1), (1, indP2)])

indP1 = indexSubspace "indS1" 0 0 7 [indP1Part]
indP2 = indexSubspace "indS2" 1 8 15 []

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
