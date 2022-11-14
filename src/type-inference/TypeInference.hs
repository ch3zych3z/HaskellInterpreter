{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module TypeInference (infereType, infereTypeExpr) where

import Control.Monad.Except
import Prelude hiding (exp)

import TIRuntime
import qualified Context
import qualified Subst
import qualified Scheme
import Ast

infixr 2 |->
(|->) :: HType -> HType -> HType
a |-> b = HTFun a b

type Infer e t = Context.Context -> e -> TI t

tiHVPat :: HValuePat -> TI HType
tiHVPat (HVPBool _) = return HTBool
tiHVPat (HVPInt _)  = return HTInt

tiPat :: HPattern -> TI (Context.Context, HType)
tiPat (HPIdent i) = do
  t <- newHTVar
  return (Context.singleton i $ Scheme.empty t, t)
tiPat HPWildcard = do
  t <- newHTVar
  return (Context.empty, t)
tiPat (HPLabel i p) = do
  (ctx, t) <- tiPat p
  return (Context.addEmpty i t ctx, t)
tiPat (HPVal v) = do
  t <- tiHVPat v
  return (Context.empty, t)

tiPats :: [HPattern] -> TI ([Context.Context], [HType])
tiPats pats = unzip <$> mapM tiPat pats


tiHValue :: HValue -> TI HType
tiHValue (HVInt _)  = return HTInt
tiHValue (HVBool _) = return HTBool
tiHValue v          = throwError $ "unknown value: " ++ show v

tiBinOp :: HBinOp -> TI HType
tiBinOp op | op `elem` [Add, Mul, Sub, Div] = return $ HTInt |-> HTInt |-> HTInt
           | op `elem` [Gr, Le, Leq, Greq]  = return $ HTInt |-> HTInt |-> HTBool
           | op == Eqls                     = do 
               t <- newHTVar 
               return $ t |-> t |-> HTBool
           | otherwise                      = throwError $ "unknown operator: " ++ show op 

tiExpr :: Infer HExpr HType
tiExpr ctx (HEVar _ i)           = Context.find i ctx >>= instantiate
tiExpr _   (HEVal v)             = tiHValue v
tiExpr ctx (HEApp f x)           = do
  tf <- tiExpr ctx f
  tx <- tiExpr ctx x
  t <- newHTVar
  unify tf $ tx |-> t
  return t
tiExpr ctx (HEAbs x e)           = do
  (as, t) <- tiPat x
  let ctx' = Context.concat as ctx
  te <- tiExpr ctx' e
  return $ t |-> te
tiExpr ctx (HEBinOp e1 op e2)    = do
  t1 <- tiExpr ctx e1
  t2 <- tiExpr ctx e2
  top <- tiBinOp op
  res <- newHTVar
  unify top $ t1 |-> t2 |-> res
  return res
tiExpr ctx (HEIf cond e1 e2)     = do
  tcond <- tiExpr ctx cond
  unify HTBool tcond
  t1 <- tiExpr ctx e1
  t2 <- tiExpr ctx e2
  unify t1 t2
  return t1
tiExpr ctx (HELet binds)         = tiBinds ctx binds
tiExpr ctx (HELetSimple f e1 e2) = do
  tf <- newHTVar
  let ctx' = Context.updateEmpty f tf ctx
--  (as, tf) <- tiPat f
--  let ctx' = Context.concat as ctx
  t1 <- tiExpr ctx' e1
  unify tf t1
  let ctx'' = Context.remove f ctx'
  s <- getSubst
  let sch2 = generalize ctx'' $ Subst.apply s tf
  let ctx''' = Context.add f sch2 ctx''
  tiExpr ctx''' e2

tiBinds :: Infer Bindings HType
tiBinds ctx (Binds bs expr) = do
  let foldBinds []                  exp = exp
      foldBinds ((Bind n e) : bnds) exp = HELetSimple n e $ foldBinds bnds exp 
  tiExpr ctx $ foldBinds bs expr

typeInferenceExpr :: Context.Context -> HExpr -> TI HType 
typeInferenceExpr ctx e = do 
  t <- tiExpr ctx e
  s <- getSubst
  return $ Subst.apply s t

infereTypeExpr :: HExpr -> Either String HType
infereTypeExpr e = runTI $ typeInferenceExpr [] e

typeInferenceBinds :: Context.Context -> Bindings -> TI HType
typeInferenceBinds ctx bs = do
  ass <- tiBinds ctx bs
  s <- getSubst
  return $ Subst.apply s ass

infereType :: HProgram -> Either String HType
infereType bs = runTI $ typeInferenceBinds [] bs
