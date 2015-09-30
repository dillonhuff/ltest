module TreeEnum(basicTreeCases) where

import Data.List as L

import Common
import TreeCase

basicTreeCases =
  L.map compileTreeCase basicCases
  
basicCase =
  treeCase "case1" (logicalRegion "lr1" (indexSpace "i1" 0 4) (fieldSpace "fs1" ["X", "Y"])) [highLevelTask "a" [regionRequirement "lr1" ["X"] RW ATOMIC "lr1"], highLevelTask "b" [regionRequirement "lr1" ["X", "Y"] RO SIMULTANEOUS "lr1"]]

basicCases = L.zipWith (\n (a, b) -> treeCase ("case" ++ show n) dlr [a, b]) [1..(length taskPairs)] taskPairs

taskPairs = cartProd (tasks "a") (tasks "b")

tasks n = L.map (\rr -> highLevelTask n [rr]) rrs

rrs = L.map (\(p, c) -> regionRequirement "lr1" ["X"] p c "lr1") taskAccesses

dlr = logicalRegion "lr1" (indexSpace "i1" 0 0) (fieldSpace "fs1" ["X"])

taskAccesses = cartProd [RO, RW] [SIMULTANEOUS, ATOMIC, EXCLUSIVE]

cartProd xs ys = [(x, y) | x <- xs, y <- ys]
