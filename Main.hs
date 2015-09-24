module Main(main) where

import CPPCode
import Syntax
import SystemUtils
import TestElaboration

main :: IO ()
main = execTestCase "first" testTasks

legionSpyPath = "/Users/dillon/CppWorkspace/Legion/legion/tools/legion_spy.py"
testPath = "/Users/dillon/Haskell/legion/ltest/cases/"
testFileName = "dummy.cc"

execTestCase :: String -> [Task] -> IO ()
execTestCase testName tasks = do
  runCommandStrict $ "mkdir " ++ testDir
  writeFile (testDir ++ "/" ++ testName ++ ".cc") (prettyCPP $ tasksToTestCase tasks)
  writeFile (testDir ++ "/Makefile") (legionMakeFile testName)
  runCommandStrict $ "make -C " ++ testDir
  runCommandStrict $ testDir ++ "/" ++ testName ++ " " ++ spyFlags
  runCommandStrict $ legionSpyPath ++ " -l spy.log"
  where
    testDir = testPath ++ testName
    spyFlags = "-level 2 -cat legion_spy -logfile spy.log"

legionMakeFile name =
  "ifndef LG_RT_DIR\n$(error LG_RT_DIR variable is not defined, aborting build)\nendif\nDEBUG=1\nOUTPUT_LEVEL=LEVEL_DEBUG\nSHARED_LOWLEVEL=1\nCC_FLAGS=-DLEGION_SPY\nOUTFILE\t:= " ++ name ++ "\nGEN_SRC\t:= " ++ name ++ ".cc" ++ "\ninclude $(LG_RT_DIR)/runtime.mk"

testTasks =
  [task "task_A" $ [regionRequirement (logicalRegion "lrA" (indexSpace "i1" 0 1) (fieldSpace "fsA" ["X", "Y"])) RW EXCLUSIVE],
   task "task_B" $ [regionRequirement (logicalRegion "lrB" (indexSpace "i2" 0 123) (fieldSpace "fsB" ["K", "Z"])) RO EXCLUSIVE]]
