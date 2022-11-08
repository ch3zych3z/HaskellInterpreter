{-# LANGUAGE FlexibleContexts #-}
module Context (module Context) where

import Control.Monad.Except

import qualified Scheme
import qualified Subst
import Ast

data Assump = String :>: Scheme.Scheme deriving (Show)

type Context = [Assump]

instance Subst.Types Assump where
  ftv (_ :>: scheme) = Subst.ftv scheme
  apply s (n :>: scheme) = n :>: Subst.apply s scheme

find :: MonadError String m => String -> Context -> m Scheme.Scheme
find n []                  = throwError $ "unbound identifier: " ++ n
find n ((i :>: sch) : ass) = if n == i then return sch else find n ass

remove :: String -> Context -> Context
remove i = filter (\(i' :>: _) -> i' /= i)

add :: String -> Scheme.Scheme -> Context -> Context
add i sch = (:) $ i :>: sch

addAll :: [String] -> [Scheme.Scheme] -> Context -> Context
addAll ss schs = (++) $ zipWith (:>:) ss schs 

addEmpty :: String -> HType -> Context -> Context
addEmpty i t = add i $ Scheme.empty t

update :: String -> Scheme.Scheme -> Context -> Context
update i sch ctx = add i sch $ remove i ctx

updateEmpty :: String -> HType -> Context -> Context
updateEmpty i t = update i $ Scheme.empty t
