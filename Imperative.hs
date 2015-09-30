module Imperative(TestCase,
                  testCase,
                  testName, testFields, testTasks,
                  Task,
                  task,
                  taskName, taskBody,
                  ImperativeStmt,
                  runtimeCall, indexSpaceInit, fieldSpaceInit, logicalRegionInit,
                  taskLaunch, indexPartitionInit,
                  impStmtToCPP) where

import Data.Char
import Data.List as L
import Data.Map as M

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
  | IndexPartitionInit String String Bool (Map Int (Int, Int))
    deriving (Eq, Ord, Show)

runtimeCall = RuntimeCall
indexPartitionInit = IndexPartitionInit
indexSpaceInit = IndexSpaceInit
fieldSpaceInit = FieldSpaceInit
logicalRegionInit = LogicalRegionInit
taskLaunch = TaskLaunch


colorBoundsCode n colorMap =
  [colorBoundsDomain]
  where
    colorBoundsDomain = objInitStmt (objectType "Domain") n domExpr
    domExpr = functionCall "Domain::from_rect" [objectType "1"] [boundRect]
    boundRect = tempObject "Rect" [objectType "1"] [startPnt, endPnt]
    startPnt = tempObject "Point" [objectType "1"] [cppVar $ show start]
    endPnt = tempObject "Point" [objectType "1"] [cppVar $ show end]
    start = L.minimum $ M.keys colorMap
    end = L.maximum $ M.keys colorMap

colorRangeInit n (c, (s, e)) =
  assignStmt (arrayRef (cppVar "") n (cppVar $ show c)) (cppVar "A")

impStmtToCPP (IndexPartitionInit name indSpaceName disjointFlag colorMap) =
  colorBounds ++ colorRanges ++ [partitionStmt]
  where
    colorBounds = colorBoundsCode (name ++ "_color_domain") colorMap
    colorRanges = (objInitStmt (objectType "DomainColoring") (name ++ "_coloring") emptyExpr):(L.map (colorRangeInit (name ++ "_coloring")) $ M.toList colorMap)
    disjointVal = cppVar $ if disjointFlag then "true" else "false"
    coloringVar = cppVar $ name ++ "_coloring"
    colorBoundsVar = cppVar $ name ++ "_color_domain"
    partitionStmt = objInitStmt (objectType "IndexPartition") name partExpr
    partExpr = ptrMethodCall runtime "create_index_partition" [] [ctx, cppVar indSpaceName, colorBoundsVar, coloringVar, disjointVal]
impStmtToCPP (RuntimeCall name args) =
  [exprStmt $ ptrMethodCall runtime name [] $ L.map cppVar args]
impStmtToCPP (IndexSpaceInit name start end) =
  [objInitStmt (objectType "IndexSpace") name indExpr]
  where
    indExpr = ptrMethodCall runtime "create_index_space" [] [ctx, dom]
    dom = functionCall "Domain::from_rect" [objectType "1"] [rect]
    rect = tempObject "Rect" [objectType "1"] [startPoint, endPoint]
    startPoint = tempObject "Point" [objectType "1"] [cppVar $ show start]
    endPoint = tempObject "Point" [objectType "1"] [cppVar $ show end]
impStmtToCPP (FieldSpaceInit name fieldNames) =
  [declSpace, fieldAllocBlock]
  where
    declSpace = objInitStmt (objectType "FieldSpace") name fsExpr
    fsExpr = ptrMethodCall runtime "create_field_space" [] [ctx]
    fieldAllocBlock = blockStmt $ fieldAllocStmts name fieldNames
impStmtToCPP (LogicalRegionInit name iName fName) =
  [objInitStmt (objectType "LogicalRegion") name lrExpr]
  where
    lrExpr = ptrMethodCall runtime "create_logical_region" [] [ctx, cppVar iName, cppVar fName]
impStmtToCPP (TaskLaunch name rrs) =
  [initTaskLauncher] ++ setRegionRequirements name rrs ++ [launchTask]
  where
    initTaskLauncher = varDeclStmt (objectType "TaskLauncher") (name ++ "_launcher") [cppVar $ L.map toUpper name, tempObject "TaskArgument" [] [cppVar "NULL", cppVar "0"]]
    launchTask = exprStmt $ ptrMethodCall runtime "execute_task" [] [ctx, cppVar $ name ++ "_launcher"]

setRegionRequirements n rrs = L.concat $ L.zipWith (buildRRCode (n ++ "_launcher")) [0..(length rrs - 1)] rrs

buildRRCode launcherName n rr = (addRR launcherName rr):(addFields launcherName n rr)

addFields launcherName n rr = L.map (addFieldCode (cppVar launcherName) n) fieldVars
  where
    fieldVars = L.map cppVar $ rrFields rr

addFieldCode launcherNameVar n fVar =
  exprStmt $ refMethodCall launcherRRN "add_field" [] [fVar]
  where
    launcherRRN = arrayRef launcherNameVar "region_requirements" (cppVar $ show n)

addRR launcherName rr =
  exprStmt $ refMethodCall (cppVar launcherName) "add_region_requirement" [] [rrObj]
  where
    rrObj = tempObject "RegionRequirement" [] [rName, privilege, coherence, rParentName]
    rName = cppVar $ rrRegion rr
    rParentName = cppVar $ rrParentRegion rr
    privilege = cppVar $ show $ rrPrivilege rr
    coherence = cppVar $ show $ rrCoherence rr

fieldAllocStmts name fieldNames =
  fieldAllocatorInit:(fieldAllocs fieldNames)
  where
    fieldAllocatorInit = objInitStmt (objectType "FieldAllocator") "allocator" faExpr
    faExpr = ptrMethodCall runtime "create_field_allocator" [] [ctx, cppVar name]

fieldAllocs fieldNames =
  L.map fieldAlloc fieldNames

fieldAlloc fieldName =
  exprStmt $ refMethodCall allocator "allocate_field" [] [functionCall "sizeof" [] [cppVar "int"], cppVar fieldName]
    

allocator = cppVar "allocator"
ctx = cppVar "ctx"
runtime = cppVar "runtime"

