module Scheme where

import qualified Data.Map as Map
import qualified Data.List as List

import qualified Subst
import Ast

data Scheme = Forall [String] HType deriving (Show)

instance Subst.Types Scheme where
  ftv (Forall ns t) = Subst.ftv t List.\\ ns
  apply s (Forall ns t) = Forall ns $ Subst.apply s' t
    where s' = foldr Map.delete s ns

quantify :: [String] -> HType -> Scheme
quantify ns t = Forall ns' t
  where ns' = List.intersect ns $ Subst.ftv t

empty :: HType -> Scheme
empty = Forall []
