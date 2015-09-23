module CPPCode(CPPTopLevelItem,
               include, namespace, enum, function,
               CPPType,
               void, int, char, ptr, ref, objectType, constq,
               templateObjectType, functionType,
               CPPStmt,
               returnStmt, exprStmt,
               CPPExpr,
               cppVar, functionCall,
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
  | ExprStmt CPPExpr
    deriving (Eq, Ord)

returnStmt = ReturnStmt
exprStmt = ExprStmt

instance Show CPPStmt where
  show (ReturnStmt expr) = "return " ++ show expr ++ ";"
  show (ExprStmt expr) = show expr ++ ";"

data CPPExpr
  = FunctionCall String [CPPType] [CPPExpr]
  | CPPVar String
    deriving (Eq, Ord)

instance Show CPPExpr where
  show (CPPVar n) = n
  show (FunctionCall str [] args) =
    str ++ showArgList args
  show (FunctionCall str tps args) =
    str ++ showTemplateParamList tps ++ showArgList args

showArgList args =
  "(" ++ (L.concat $ L.intersperse ", " $ L.map show args) ++ ")"

functionCall = FunctionCall
cppVar = CPPVar

prettyCPP :: [CPPTopLevelItem] -> String
prettyCPP items = L.concat $ L.intersperse "\n" $ L.map show items

