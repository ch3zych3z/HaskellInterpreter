module Pattern (module Pattern) where

import qualified Data.List as List (elem)

import Ast
import Runtime
  
class Matchable a where
  match :: a -> HExpr -> Runtime (Maybe Scope)

collectIdents :: [HPattern] -> [HId]
collectIdents = let
  helper (HPIdent i)    acc = i : acc
  helper (HPLabel is p) acc = helper p (is ++ acc)
  helper (HPList p)     acc = 
    case p of
      HLPNil        -> []
      HLPCons hp tp -> helper hp $ helper tp acc
  helper _              acc = acc
  in foldr helper []
  
elem :: HId -> HPattern -> Bool
elem i p = List.elem i $ collectIdents [p]
