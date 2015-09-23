module Main(main) where

import CPPCode
import Syntax

main :: IO ()
main =
  putStrLn $ prettyCPP $ tasksToTestCase testTasks

testTasks =
  [task "task_A" $ [regionRequirement (logicalRegion (indexSpace 0 1) (fieldSpace ["X", "Y"])) RW EXCLUSIVE],
   task "task_B" $ [regionRequirement (logicalRegion (indexSpace 0 123) (fieldSpace ["K", "Z"])) RO EXCLUSIVE]]
