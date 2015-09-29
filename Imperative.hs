module Imperative(TestCase,
                  testCase,
                  testName, testFields, testTasks,
                  Task,
                  task,
                  taskName, taskBody,
                  ImperativeStmt,
                  impStmtToCPP) where

import Data.List as L

import Common
import CPPCode

data TestCase
  = TestCase {
    testName :: String,
    testFields :: [String],
    testTasks :: [Task]
    } deriving (Eq, Ord, Show)

testCase = TestCase

data Task
  = Task {
    taskName :: String,
    taskBody :: [ImperativeStmt]
    } deriving (Eq, Ord, Show)

task = Task

data ImperativeStmt
  = RuntimeCall String [String]
  | IndexSpaceInit String Int Int
  | FieldSpaceInit String [String]
  | LogicalRegionInit String String String
  | TaskLaunch String [RegionRequirement]
    deriving (Eq, Ord, Show)

impStmtToCPP (RuntimeCall name args) =
  [exprStmt $ ptrMethodCall runtime name [] $ L.map cppVar args]

allocator = cppVar "allocator"
ctx = cppVar "ctx"
runtime = cppVar "runtime"

