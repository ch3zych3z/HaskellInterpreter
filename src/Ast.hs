module Ast (module Ast) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Text.PrettyPrint as PP

type HId = String

data HType =
    HTInt
  | HTBool
  | HTList HType
  | HTTuple Int [HType]
  | HTMaybe HType
  | HTFun HType HType
  | HTVar String
  deriving (Eq, Ord)

data HValue =
    HVInt Int
  | HVBool Bool
  deriving Eq

data HBinOp =
    Add
  | Sub
  | Mul
  | Div
  | Rem

  | Eqls

  | Gr
  | Le
  | Greq
  | Leq
  deriving (Eq, Show)

data HList =
    HLNil
  | HLCons HExpr HExpr
  deriving (Eq, Show)

data HTypeCons =
    HCList HList
  | HCTuple Int [HExpr]
  | HCMaybe (Maybe HExpr)
  deriving (Eq, Show)

listExpr :: HList -> HExpr
listExpr = HETypeCons . HCList

nilExpr :: HExpr
nilExpr = listExpr HLNil

consExpr :: HExpr -> HExpr -> HExpr
consExpr h t = listExpr $ HLCons h t

data Binding = Bind HId HExpr deriving (Eq, Show)

type Scope = Map.Map HId HExpr

data Matching = HPattern :->: HExpr deriving (Eq, Show)

data HExpr =
    HEVal HValue
  | HEVar Scope HId -- x
  | HEApp HExpr HExpr
  | HEAbs HPattern HExpr
  | HELet Bindings
  | HEBinOp HExpr HBinOp HExpr
  | HEIf HExpr HExpr HExpr
  | HECase HExpr [Matching]
  | HETypeCons HTypeCons
  deriving (Eq, Show)

data Bindings = Binds [Binding] HExpr deriving (Eq, Show)

type HProgram = Bindings

-- Patterns

data HValuePat = 
    HVPInt Int
  | HVPBool Bool
  deriving (Eq, Show)

data HListPat =
    HLPNil
  | HLPCons HPattern HPattern
  deriving (Eq, Show)

data HPattern =
    HPIdent HId
  | HPLabel [HId] HPattern
  | HPVal HValuePat
  | HPList HListPat
  | HPTuple Int [HPattern]
  | HPMaybe (Maybe HPattern)
  | HPWildcard
  deriving (Eq, Show)

-- Pretty printers

data HShow =
    HSBool Bool
  | HSInt Int
  | HSList [HShow]
  | HSTuple [HShow]
  | HSMaybe (Maybe HShow)

instance Show HType where 
  showsPrec _ x = shows (prType x)

prType :: HType -> PP.Doc
prType (HTVar n)      = PP.text n
prType HTInt          = PP.text "Int"
prType HTBool         = PP.text "Bool"
prType (HTList t)     = PP.text $ "[" ++ show t ++ "]"
prType (HTTuple _ ts) = PP.text $ "(" ++ List.intercalate "," (map show ts) ++ ")"
prType (HTMaybe t)    = PP.text "Maybe" PP.<+> prParenType t
prType (HTFun t s)    = prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType :: HType -> PP.Doc 
prParenType t = case t of
  HTFun _ _ -> PP.parens (prType t) 
  HTMaybe _ -> PP.parens (prType t)
  _         -> prType t

instance Show HValue where
  show (HVInt v)  = show v
  show (HVBool v) = show v

instance Show HShow where
  show (HSBool b)   = show b
  show (HSInt i)    = show i
  show (HSList xs)  = show xs
  show (HSTuple xs) = "(" ++ List.intercalate "," (map show xs) ++ ")"
  show (HSMaybe v)  =
    case v of
      Just s  -> "Just " ++ show s
      Nothing -> "Nothing"
