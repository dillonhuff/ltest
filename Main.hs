module Main(main) where

import Data.Char
import Data.List as L

import CPPCode
import Syntax
import SystemUtils
import TestElaboration

main :: IO ()
main = do
  sequence_ $ L.map (\(testName, tasks) -> execTestCase testName tasks) tests
  sequence_ $ L.map (\(testName, _) -> showTestResult testName) tests

legionSpyPath = "/Users/dillon/CppWorkspace/Legion/legion/tools/legion_spy.py"
testPath = "/Users/dillon/Haskell/legion/ltest/cases/"

execTestCase :: String -> [Task] -> IO ()
execTestCase testName tasks = do
  runCommandStrict $ "mkdir " ++ testDir
  writeFile (testDir ++ "/" ++ testName ++ ".cc") (prettyCPP $ tasksToTestCase tasks)
  writeFile (testDir ++ "/Makefile") (legionMakeFile testName)
  runCommandStrict $ "make -C " ++ testDir
  runCommandStrict $ testDir ++ "/" ++ testName ++ " " ++ spyFlags
  runCommandStrict $ legionSpyPath ++ " -l " ++ spyFile ++ " > " ++ testResFile
  where
    testDir = testPath ++ testName
    spyFlags = "-level 2 -cat legion_spy -logfile " ++ spyFile
    spyFile = testDir ++ "/spy.log"
    testResFile = testDir ++ "/result.txt"
  
legionMakeFile name =
  "ifndef LG_RT_DIR\n$(error LG_RT_DIR variable is not defined, aborting build)\nendif\nDEBUG=1\nOUTPUT_LEVEL=LEVEL_DEBUG\nSHARED_LOWLEVEL=1\nCC_FLAGS=-DLEGION_SPY\nOUTFILE\t:= " ++ name ++ "\nGEN_SRC\t:= " ++ name ++ ".cc" ++ "\ninclude $(LG_RT_DIR)/runtime.mk"

showTestResult :: String -> IO ()
showTestResult testName = do
  res <- parseTestResult testName
  putStrLn res

parseTestResult :: String -> IO String
parseTestResult testName = do
  legionSpyOutput <- readFile $ testPath ++ "/" ++ testName ++ "/result.txt"
  return $ parseLegionSpyLog legionSpyOutput

parseLegionSpyLog :: String -> String
parseLegionSpyLog logStr =
  let logLines = lines logStr
      lineSuffixes = map (dropWhile isSpace) logLines in
   head $ filter (isInfixOf "Mapping Dependence Errors:") lineSuffixes

tests = [("test1", testTasks1), ("test2", testTasks2)]

testTasks1 =
  [task "task_A" $ [regionRequirement (logicalRegion "lrA" (indexSpace "i1" 0 1) (fieldSpace "fsA" ["X", "Y"])) ["X"] RW EXCLUSIVE],
   task "task_B" $ [regionRequirement (logicalRegion "lrB" (indexSpace "i2" 0 123) (fieldSpace "fsB" ["K", "Z"])) ["K", "Z"] RO EXCLUSIVE]]

testTasks2 =
  [task "task_A" $ [regionRequirement (logicalRegion "lrA" (indexSpace "i1" 0 1) (fieldSpace "fsA" ["X", "Y"])) ["X", "Y"] RW EXCLUSIVE],
   task "task_B" $ [regionRequirement (logicalRegion "lrA" (indexSpace "i1" 0 1) (fieldSpace "fsA" ["X", "Y"])) ["X"] RW EXCLUSIVE]]
