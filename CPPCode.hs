module CPPCode(CPPTopLevelItem,
               include, namespace, enum, function,
               CPPType,
               void, int, char, ptr, ref, objectType, constq,
               templateObjectType, functionType,
               CPPStmt,
               objInitStmt, returnStmt, exprStmt, blockStmt,
               CPPExpr,
               cppVar, functionCall, ptrMethodCall, tempObject, refMethodCall,
               prettyCPP) where

import Data.List as L

data CPPTopLevelItem
  = Include String
  | Namespace String
  | Enum String [String]
  | Function CPPType String [(CPPType, String)] [CPPStmt]
    deriving (Eq, Ord)

include str = Include $ "\"" ++ str ++ "\""
namespace = Namespace
enum = Enum
function = Function

instance Show CPPTopLevelItem where
  show (Include str) = "#include " ++ str
  show (Namespace str) = "using namespace " ++ str ++ ";"
  show f@(Function _ _ _ _) = showFunction f
  show (Enum enumName values) =
    "enum " ++ enumName ++ " {" ++
    (L.concat $ L.intersperse ", " values)
    ++ "};"

showFunction (Function tp name formalParams body) =
  show tp ++ " " ++ name ++ showParamList formalParams ++ showBody body

showBody stmts =
  "{\n" ++ (L.concatMap (\stmt -> show stmt ++ "\n") stmts) ++ "\n}"

showParamList ps =
  "(" ++ (L.concat $ L.intersperse ", " $ L.map showParam ps) ++ ")"

showParam (t, n) = show t ++ " " ++ n

data CPPType
  = Void
  | Int
  | Char
  | Ptr CPPType
  | Ref CPPType
  | QualifiedType Qualifier CPPType
  | ObjectType String
  | FunctionType String
  | TemplateObjectType String [CPPType]
    deriving (Eq, Ord)

void = Void
int = Int
char = Char
ptr = Ptr
ref = Ref
const = Const
constq = QualifiedType Const
objectType = ObjectType
functionType = FunctionType
templateObjectType = TemplateObjectType

instance Show CPPType where
  show Void = "void"
  show Int = "int"
  show Char = "char"
  show (Ptr t) = show t ++ "*"
  show (Ref t) = show t ++ "&"
  show (ObjectType n) = n
  show (FunctionType n) = n
  show (TemplateObjectType n ts) = n ++ showTemplateParamList ts
  show (QualifiedType q t) = show q ++ " " ++ show t

showTemplateParamList tps =
  "<" ++ (L.concat $ L.intersperse ", " $ L.map show tps) ++ ">"

data Qualifier
  = Const
    deriving (Eq, Ord)

instance Show Qualifier where
  show Const = "const"

data CPPStmt
  = ReturnStmt CPPExpr
  | ObjInitStmt CPPType String CPPExpr
  | ExprStmt CPPExpr
  | BlockStmt [CPPStmt]
    deriving (Eq, Ord)

blockStmt = BlockStmt
objInitStmt = ObjInitStmt
returnStmt = ReturnStmt
exprStmt = ExprStmt

instance Show CPPStmt where
  show (ObjInitStmt t n e) = show t ++ " " ++ n ++ " = " ++ show e ++ ";"
  show (BlockStmt stmts) = "{\n" ++ (L.concat $ L.intersperse "\n" $ L.map show stmts) ++ "\n}"
  show (ReturnStmt expr) = "return " ++ show expr ++ ";"
  show (ExprStmt expr) = show expr ++ ";"

data CPPExpr
  = FunctionCall String [CPPType] [CPPExpr]
  | PtrMethodCall CPPExpr String [CPPType] [CPPExpr]
  | RefMethodCall CPPExpr String [CPPType] [CPPExpr]
  | CPPVar String
  | TempObject String [CPPType] [CPPExpr]
    deriving (Eq, Ord)

instance Show CPPExpr where
  show (CPPVar n) = n
  show (TempObject n tps args) = n ++ showTemplateParamList tps ++ showArgList args
  show (PtrMethodCall e n tps args) = show e ++ "->" ++ show (FunctionCall n tps args)
  show (RefMethodCall e n tps args) = show e ++ "." ++ show (FunctionCall n tps args)
  show (FunctionCall str [] args) =
    str ++ showArgList args
  show (FunctionCall str tps args) =
    str ++ showTemplateParamList tps ++ showArgList args

showArgList args =
  "(" ++ (L.concat $ L.intersperse ", " $ L.map show args) ++ ")"

tempObject = TempObject
refMethodCall = RefMethodCall
ptrMethodCall = PtrMethodCall
functionCall = FunctionCall
cppVar = CPPVar

prettyCPP :: [CPPTopLevelItem] -> String
prettyCPP items = L.concat $ L.intersperse "\n\n" $ L.map show items

