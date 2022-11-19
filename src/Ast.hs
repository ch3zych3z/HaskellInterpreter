module Ast (module Ast) where

import qualified Data.Map as Map
import qualified Text.PrettyPrint as PP

type HId = String

data HType =
    HTInt
  | HTBool
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

  | Eqls

  | Gr
  | Le
  | Greq
  | Leq
  deriving (Eq, Show)

data Binding = Bind HId HExpr deriving (Eq, Show)

type Scope = Map.Map HId HExpr

data Matching = HPattern :->: HExpr deriving (Eq, Show)

data HExpr =
    HEVal HValue
  | HEVar Scope HId -- x
  | HEApp HExpr HExpr
  | HEAbs HPattern HExpr
  | HELet Bindings
  | HELetSimple HId HExpr HExpr
  | HEBinOp HExpr HBinOp HExpr
  | HEIf HExpr HExpr HExpr
  | HECase HExpr [Matching]
  deriving (Eq, Show)

data Bindings = Binds [Binding] HExpr deriving (Eq, Show)

type HProgram = Bindings

-- Patterns

data HValuePat = 
    HVPInt Int
  | HVPBool Bool
  deriving (Eq, Show)

data HPattern =
    HPIdent HId
  | HPLabel HId HPattern
  | HPVal HValuePat
  | HPWildcard
  deriving (Eq, Show)

-- Pretty printers

instance Show HType where 
  showsPrec _ x = shows (prType x)

prType :: HType -> PP.Doc
prType (HTVar n)   = PP.text n
prType HTInt       = PP.text "Int"
prType HTBool      = PP.text "Bool"
prType (HTFun t s) = prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType :: HType -> PP.Doc 
prParenType t = case t of
  HTFun _ _ -> PP.parens (prType t) 
  _ -> prType t

instance Show HValue where
  show (HVInt v)  = show v
  show (HVBool v) = show v
