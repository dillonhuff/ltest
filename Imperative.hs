module Imperative(TestCase,
                  testCase,
                  testName, testFields, testTasks,
                  Task,
                  task,
                  taskName, taskBody,
                  ImperativeStmt,
                  impStmtToCPP,
                  Coherence(..),
                  Privilege(..)) where

import Data.List as L

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

data RegionRequirement
  = RegionRequirement {
    rrRegion :: String,
    rrPrivilege :: Privilege,
    rrCoherence :: Coherence,
    rrParentRegion :: String
    } deriving (Eq, Ord, Show)

data Privilege
  = RW
  | RO
    deriving (Eq, Ord)

instance Show Privilege where
  show RW = "READ_WRITE"
  show RO = "READ_ONLY"

data Coherence
  = ATOMIC
  | SIMULTANEOUS
  | EXCLUSIVE
    deriving (Eq, Ord, Show)
