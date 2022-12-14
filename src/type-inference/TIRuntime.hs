{-# LANGUAGE FlexibleContexts #-}
module TIRuntime (module TIRuntime) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Subst
import qualified Scheme
import qualified Context
import Ast

varBind :: MonadError String m => String -> HType -> m Subst.Subst
varBind u t | t == HTVar u              = return Subst.empty
            | u `List.elem` Subst.ftv t = throwError $ "occurs check fails: " ++ u ++ " ~ " ++ show t
            | otherwise                 = return $ Map.singleton u t

mgu :: MonadError String m => HType -> HType -> m Subst.Subst
mgu (HTFun l r) (HTFun l' r') = do 
  s1 <- mgu l l'
  s2 <- mgu (Subst.apply s1 r) (Subst.apply s1 r')
  return $ s1 `Subst.compose` s2
mgu (HTVar n) t                                  = varBind n t
mgu t (HTVar n)                                  = varBind n t
mgu (HTList t1) (HTList t2)                      = mgu t1 t2
mgu (HTTuple l1 ts1) (HTTuple l2 ts2) | l1 == l2 = do
  ss <- zipWithM mgu ts1 ts2
  return $ foldl1 Subst.compose ss
mgu (HTMaybe t1) (HTMaybe t2)                    = mgu t1 t2
mgu t1 t2 | t1 == t2                             = return Subst.empty
          | otherwise                            = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

data TIRuntime = TIRuntime {
  supply  :: Int
, subst   :: Subst.Subst
}

type TI a = ExceptT String (State TIRuntime) a

runTI :: TI a -> Either String a
runTI t = let 
  (res, _) = runState (runExceptT t) initTIState 
  initTIState = TIRuntime { supply = 0, subst = Subst.empty }
  in res

getSubst :: TI Subst.Subst
getSubst = subst <$> get

extSubst :: Subst.Subst -> TI ()
extSubst s' = modify $ \rt -> rt { subst = Subst.compose s' $ subst rt }

unify :: HType -> HType -> TI ()
unify t1 t2 = do
  s <- getSubst
  u <- mgu (Subst.apply s t1) (Subst.apply s t2)
  extSubst u

unifyAll :: [HType] -> TI ()
unifyAll ts = forM_ ts $ \t1 -> forM_ ts $ \t2 -> unify t1 t2

newHTVar :: TI HType
newHTVar = do
  n <- supply <$> get
  modify $ \rt -> rt { supply = n + 1 }
  return $ HTVar $ 'a' : show n

instantiate :: Scheme.Scheme -> TI HType
instantiate (Scheme.Forall vars t) = do 
  nvars <- mapM (const newHTVar) vars
  let s = Map.fromList $ zip vars nvars
  return $ Subst.apply s t

generalize :: Context.Context -> HType -> Scheme.Scheme
generalize ctx t = Scheme.Forall vars t
  where vars = Subst.ftv t List.\\ Subst.ftv ctx
