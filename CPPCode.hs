module CPPCode(CPPTopLevelItem,
               include, namespace, enum, function,
               CPPType,
               void, int, char, ptr, objectType,
               prettyCPP) where

import Data.List as L

data CPPTopLevelItem
  = Include String
  | Namespace String
  | Enum String [String]
  | Function CPPType String [(CPPType, String)]
    deriving (Eq, Ord)

include str = Include $ "\"" ++ str ++ "\""
namespace = Namespace
enum = Enum
function = Function

instance Show CPPTopLevelItem where
  show (Include str) = "#include " ++ str
  show (Namespace str) = "using namespace " ++ str ++ ";"
  show f@(Function tp name formalParams) = showFunction f
  show (Enum enumName values) =
    "enum " ++ enumName ++ " {" ++
    (L.concat $ L.intersperse ", " values)
    ++ "};"

showFunction (Function tp name formalParams) =
  show tp ++ " " ++ name ++ showParamList formalParams ++ "{}"

showParamList ps =
  "(" ++ (L.concat $ L.intersperse ", " $ L.map showParam ps) ++ ")"

showParam (t, n) = show t ++ " " ++ n

data CPPType
  = Void
  | Int
  | Char
  | Ptr CPPType
  | ObjectType String
    deriving (Eq, Ord)

void = Void
int = Int
char = Char
ptr = Ptr
objectType = ObjectType

instance Show CPPType where
  show Void = "void"
  show Int = "int"
  show Char = "char"
  show (Ptr t) = show t ++ "*"
  show (ObjectType n) = n

prettyCPP :: [CPPTopLevelItem] -> String
prettyCPP items = L.concat $ L.intersperse "\n" $ L.map show items

