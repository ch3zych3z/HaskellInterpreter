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

data HExpr =
    HEVal HValue
  | HEVar Scope HId -- x
  | HEApp HExpr HExpr
  | HEAbs HId HExpr
  | HELet Bindings
  | HELetSimple HId HExpr HExpr
  | HEBinOp HExpr HBinOp HExpr
  | HEIf HExpr HExpr HExpr
  deriving (Eq, Show)

data Bindings = Binds [Binding] HExpr deriving (Eq, Show)

type HProgram = Bindings

-- Patterns

class Matchable a where
  matches :: a -> HExpr -> Bool

data HValuePat = 
    HVPInt Int
  | HVPBool Bool
  deriving (Eq, Show)

instance Matchable HValuePat where
  matches (HVPInt v1)  (HEVal (HVInt v2))  = v1 == v2
  matches (HVPBool v1) (HEVal (HVBool v2)) = v1 == v2
  matches _            _                   = False

data HPattern =
    HPIdent HId
  | HPLabel HId HPattern
  | HPVal HValuePat
  | HPWildcard
  deriving (Eq, Show)

instance Matchable HPattern where
  matches (HPIdent _)   _ = True
  matches (HPLabel _ p) e = matches p e
  matches HPWildcard    _ = True
  matches (HPVal p)     e = matches p e

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
