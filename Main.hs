module Main(main) where

import Data.Char
import Data.List as L

import CPPCode
import Imperative
import SystemUtils
import TaskEnum
import TestElaboration

main :: IO ()
main = do
  sequence_ $ L.map (\(testName, tasks) -> execTestCase testName tasks) twoTaskTests
  sequence_ $ L.map (\(testName, _) -> showTestResult testName) twoTaskTests

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
   case filter (isInfixOf "Mapping Dependence Errors:") lineSuffixes of
    [l] -> l
    _ -> "ERROR: Could not find mapping dependence line"

twoTaskTests = L.zip (L.map (\i -> "test" ++ show i) [1..(length basicTasks)]) basicTasks
