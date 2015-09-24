module Main(main) where

import CPPCode
import Syntax

testDir = "/Users/dillon/Haskell/legion/ltest/dummy/"
testFileName = "dummy.cc"

main :: IO ()
main =
  writeFile (testDir ++ testFileName) (prettyCPP $ tasksToTestCase testTasks)

testTasks =
  [task "task_A" $ [regionRequirement (logicalRegion (indexSpace 0 1) (fieldSpace ["X", "Y"])) RW EXCLUSIVE],
   task "task_B" $ [regionRequirement (logicalRegion (indexSpace 0 123) (fieldSpace ["K", "Z"])) RO EXCLUSIVE]]
