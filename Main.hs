module Main(main) where

import CPPCode
import Syntax
import TestElaboration

testDir = "/Users/dillon/Haskell/legion/ltest/dummy/"
testFileName = "dummy.cc"

main :: IO ()
main =
  writeFile (testDir ++ testFileName) (prettyCPP $ tasksToTestCase testTasks)

testTasks =
  [task "task_A" $ [regionRequirement (logicalRegion "lrA" (indexSpace "i1" 0 1) (fieldSpace "fsA" ["X", "Y"])) RW EXCLUSIVE],
   task "task_B" $ [regionRequirement (logicalRegion "lrB" (indexSpace "i2" 0 123) (fieldSpace "fsB" ["K", "Z"])) RO EXCLUSIVE]]
