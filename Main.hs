module Main(main) where

import Data.Char
import Data.List as L

import CPPCode
import Imperative
import SystemUtils
import TestElaboration
import TreeCase
import TreeEnum

main :: IO ()
main = do
  sequence_ $ L.map execTestCase basicTreeCases
  results <- sequence $ L.map (\t -> getTestResult $ testName t) basicTreeCases
  putStrLn $ showTestSuiteResults results

legionSpyPath = "/Users/dillon/CppWorkspace/Legion/Master/legion/tools/legion_spy.py"
testPath = "/Users/dillon/Haskell/legion/ltest/suite20/"

execTestCase :: TestCase -> IO ()
execTestCase testCase = do
  runCommandStrict $ "mkdir " ++ testDir
  writeFile (testDir ++ "/" ++ n ++ ".cc") (prettyCPP $ toCPP testCase)
  writeFile (testDir ++ "/Makefile") (legionMakeFile n)
  runCommandStrict $ "make -C " ++ testDir
  runCommandStrict $ testDir ++ "/" ++ n ++ " " ++ spyFlags
  runCommandStrict $ legionSpyPath ++ " -l " ++ spyFile ++ " > " ++ testResFile
  where
    testDir = testPath ++ n
    spyFlags = "-level 2 -cat legion_spy -logfile " ++ spyFile
    spyFile = testDir ++ "/spy.log"
    testResFile = testDir ++ "/result.txt"
    n = testName testCase
  
legionMakeFile name =
  "ifndef LG_RT_DIR\n$(error LG_RT_DIR variable is not defined, aborting build)\nendif\nDEBUG=1\nOUTPUT_LEVEL=LEVEL_DEBUG\nSHARED_LOWLEVEL=1\nCC_FLAGS=-DLEGION_SPY\nOUTFILE\t:= " ++ name ++ "\nGEN_SRC\t:= " ++ name ++ ".cc" ++ "\ninclude $(LG_RT_DIR)/runtime.mk"

showTestSuiteResults :: [(String, Int)] -> String
showTestSuiteResults cases =
  let numCases = L.length cases
      unparseableCases = L.filter (\(_, n) -> n == -1) cases
      depAnalysisFailedCases = L.filter (\(_, n) -> n > 0) cases in
  "Number of cases:            " ++ show numCases ++ "\n" ++
  "Unparseable:                " ++ show (L.length unparseableCases) ++ "\n" ++
  "Dependence Analysis Errors: " ++ show (L.length depAnalysisFailedCases) ++ "\n" ++
  "\nCases\t\t# of dependence errors\n" ++
  "-------------------------------------------------" ++ "\n" ++
  (L.concat $ L.intersperse "\n" $ L.map showCase depAnalysisFailedCases)
  where
    showCase (name, e) = name ++ "\t\t" ++ show e

numTests cases = "Total tests:\t" ++ (show $ L.length cases)

numUnparseable cases =
  "Unparseable:\t" ++ (show $ L.length $ L.filter (\(_, n) -> n == (-1)) cases)

failedDepTests cases =
  "Failed:\t" ++ (show $ L.filter (\(_, n) -> n > 0) cases)

getTestResult :: String -> IO (String, Int)
getTestResult testName = do
  res <- parseTestResult testName
  return (testName, res)

parseTestResult :: String -> IO Int
parseTestResult testName = do
  legionSpyOutput <- readFile $ testPath ++ "/" ++ testName ++ "/result.txt"
  return $ parseLegionSpyLog legionSpyOutput

parseLegionSpyLog :: String -> Int
parseLegionSpyLog logStr =
  let logLines = lines logStr
      lineSuffixes = L.map (dropWhile isSpace) logLines
      mapDepErrs = L.head $ L.filter (isInfixOf "Mapping Dependence Errors: ") lineSuffixes
      numErrs = L.reverse $ L.takeWhile isDigit $ L.reverse mapDepErrs in
   case numErrs of
    "" -> -1
    _ -> read $ numErrs
