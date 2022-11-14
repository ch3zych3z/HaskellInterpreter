module Pattern (module Pattern) where

import qualified Data.List as List (elem)
import qualified Data.Map as Map 

import Ast
  
class Matchable a where
  matches :: a -> HExpr -> Bool
  match :: a -> HExpr -> Scope

instance Matchable HValuePat where
  matches (HVPInt v1)  (HEVal (HVInt v2))  = v1 == v2
  matches (HVPBool v1) (HEVal (HVBool v2)) = v1 == v2
  matches _            _                   = False
  
  match _ _ = Map.empty

instance Matchable HPattern where
  matches (HPIdent _)   _ = True
  matches (HPLabel _ p) e = matches p e
  matches HPWildcard    _ = True
  matches (HPVal p)     e = matches p e
  
  match (HPIdent i)   e = Map.singleton i e
  match (HPLabel i p) e = Map.singleton i e `Map.union` match p e
  match HPWildcard    _ = Map.empty
  match (HPVal v)     e = match v e

collectIdents :: [HPattern] -> [HId]
collectIdents = let
  helper (HPIdent i)   acc = i : acc
  helper (HPLabel i p) acc = helper p (i : acc)
  helper _             acc = acc
  in concatMap (`helper` [])
  
elem :: HId -> HPattern -> Bool
elem i p = List.elem i $ collectIdents [p]
