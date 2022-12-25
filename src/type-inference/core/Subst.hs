{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Subst (module Subst) where

import qualified Data.Map as Map
import qualified Data.List as List

import Ast

type Subst = Map.Map String HType

class Types a where
  ftv :: a -> [String]
  apply :: Subst -> a -> a

instance Types a => Types [a] where
  ftv = foldr (List.union . ftv) []
  apply s = map $ apply s

instance Types HType where
  ftv = \case 
    HTInt       -> []
    HTBool      -> []
    HTFun t1 t2 -> ftv t1 `List.union` ftv t2
    HTList t    -> ftv t
    HTVar n     -> [n]
    HTTuple _ t -> ftv t
    HTMaybe t   -> ftv t
  apply s = \case 
    var@(HTVar n) -> Map.findWithDefault var n s
    HTFun t1 t2   -> HTFun (apply s t1) (apply s t2)
    HTList t      -> HTList $ apply s t
    HTTuple l t   -> HTTuple l $ map (apply s) t
    HTMaybe t     -> HTMaybe $ apply s t
    t             -> t

empty :: Subst
empty = Map.empty 

compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1
