{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Interpreter (interpret) where

import qualified Data.Map as Map
import Prelude hiding (exp)
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace

import Pattern
import Ast
import Runtime
import qualified Exception

unreachable :: a
unreachable = error "error: unreachable code fragment"

incorrectOperationType :: HBinOp -> String -> a
incorrectOperationType op t = error $ "error: " ++ show op ++ " is not type of " ++ t

-- Pattern matching

reduce2match :: HPattern -> HExpr -> Runtime (Maybe HExpr)
reduce2match p e | isWHNF e && not (matches p e) = return Nothing
                 | matches p e                     = return $ Just e
                 | otherwise                       = do
                   e' <- reduce e
                   reduce2match p e'

-- Helpers

bindsContains :: [Binding] -> HId -> Bool
bindsContains bs i = isJust $ List.find (i ==) $ map (\(Bind n _) -> n) bs

substBinds :: [Binding] -> HId -> HExpr -> [Binding]
substBinds bs ident exp =
  let substBind b@(Bind i e) | i /= ident = Bind i $ subst e ident exp
                             | otherwise  = b
  in map substBind bs

subst :: HExpr -> HId -> HExpr -> HExpr
subst ab@(HEAbs n to)         i e  | i `Pattern.elem` n = ab
                                   | otherwise          = HEAbs n $ subst to i e
subst v@(HEVar _ n)           i e  | n == i             = e
                                   | otherwise          = v
subst v@(HEVal _)             _ _                       = v
subst (HEApp e1 e2)           i e                       = HEApp (subst e1 i e) (subst e2 i e)
subst l@(HELet (Binds bs e1)) i e2 | bindsContains bs i = l
                                   | otherwise          = HELet $ Binds (substBinds bs i e2) $ subst e1 i e2
subst (HEBinOp e1 op e2)      i e                       = HEBinOp (subst e1 i e) op (subst e2 i e)
subst (HEIf e1 e2 e3)         i e                       = HEIf (subst e1 i e) (subst e2 i e) (subst e3 i e)
subst HELetSimple {}          _ _                       = unreachable

setScopes :: Scope -> HExpr -> HExpr
setScopes scope v@(HEVar sc n) | Map.member n scope &&
                                 Map.null sc        = HEVar scope n
                               | otherwise          = v
setScopes _     v@(HEVal _)                         = v
setScopes sc      (HEApp e1 e2)                     = HEApp (setScopes sc e1) (setScopes sc e2)
setScopes sc      (HEAbs n e)                       = HEAbs n $ setScopes sc e
setScopes sc      (HELet (Binds bs e))              =
  let setScopesBinds = map $ \(Bind i exp) -> Bind i $ setScopes sc' exp
      sc'            = sc Map.\\ binds2Scope bs
  in HELet $ Binds (setScopesBinds bs) $ setScopes sc' e
setScopes sc      (HEBinOp e1 op e2)                = HEBinOp (setScopes sc e1) op (setScopes sc e2)
setScopes sc      (HEIf e1 e2 e3)                   = HEIf (setScopes sc e1) (setScopes sc e2) (setScopes sc e3)
setScopes _       _                                 = unreachable

apply :: HExpr -> HExpr -> Runtime HExpr
apply (HEAbs p to) e = do
  matched <- reduce2match p e
  case matched of
    Just e' -> return $ Map.foldlWithKey subst to $ match p e'
    Nothing -> Exception.incompletePM
apply _ _            = trace "apply" unreachable

toArithm :: HBinOp -> Int -> Int -> Int
toArithm = \case
  Add -> (+)
  Mul -> (*)
  Sub -> (-)
  Div -> div
  op  -> incorrectOperationType op "Arithmetics"

toOrd :: HBinOp -> Int -> Int -> Bool
toOrd = \case
  Gr   -> (>)
  Le   -> (<)
  Greq -> (>=)
  Leq  -> (<=)
  op   -> incorrectOperationType op "Orders"

-- Patterns for laziness

data ExprKind =
    WHNF
  | Abstraction

instance Matchable ExprKind where
  matches WHNF    (HEVal _)       = True
  matches Abstraction (HEAbs _ _) = True
  matches _           _           = False
  match _ _ = Map.empty

isWHNF :: HExpr -> Bool
isWHNF = matches WHNF

-- Interpreter

reduce2kind :: ExprKind -> HExpr -> Runtime HExpr
reduce2kind k e | matches k e = return e
                | otherwise   = reduce e >>= reduce2kind k

reduce2whnf :: HExpr -> Runtime HValue
reduce2whnf e = do
  e' <- reduce2kind WHNF e
  case e' of
    HEVal v -> return v
    _       -> unreachable

reduce2abs :: HExpr -> Runtime HExpr
reduce2abs = reduce2kind Abstraction

reduceOp :: HBinOp -> HValue -> HValue -> Runtime HExpr
reduceOp op v1 v2
  | op `List.elem` [Add, Mul, Sub, Div] = do
    let (HVInt a) = v1
        (HVInt b) = v2
    if b == 0 && op == Div then
      Exception.throwZeroDivision
    else
      return $ HEVal $ HVInt $ toArithm op a b
  | op == Eqls                          =
    return $ HEVal $ HVBool $ v1 == v2
  | op `List.elem` [Gr, Le, Greq, Leq]  = do
    let (HVInt a) = v1
        (HVInt b) = v2
    return $ HEVal $ HVBool $ toOrd op a b
reduceOp _ _ _                          = trace "reduce op" unreachable

reduce :: HExpr -> Runtime HExpr
reduce = \case
  HEApp e1 e2         -> do
    -- traceM $ "reducing application:\n" ++ show (HEApp e1 e2)
    e1' <- reduce2abs e1
    apply e1' e2
  val@(HEVal _)       -> {-traceM ("reducing val:\n" ++ show val) >>-} return val
  v@(HEVar sc i)      -> do
    -- traceM ("reducing var:\n" ++ show v)
    pushScope sc
    e <- setScopes sc . fromMaybe v <$> getVar i
    -- kek <- fromMaybe v <$> getVar i
    -- traceM $ "unscoped var:\n" ++ show kek
    -- traceM $ "scoped var:\n" ++ show e
    popScope
    return e
  a@(HEAbs _ _)       -> {-traceM ("reducing abstraction:\n" ++ show a) >> -}return a
  HELet (Binds bs e)  -> do
    -- traceM $ "reducing let:\n" ++ show (Binds bs e)
    let sc = binds2Scope bs
    -- traceM $ "bindings to be closured:\n" ++ show bs
    -- traceM $ "closured by\n" ++ show sc
    return $ setScopes sc e
  HEBinOp e1 op e2    -> do
    -- traceM $ "reducing binop:\n" ++ show (HEBinOp e1 op e2)
    e1' <- reduce2whnf e1
    e2' <- reduce2whnf e2
    reduceOp op e1' e2'
  HEIf cond e1 e2     -> do
    -- traceM $ "reducing if:\n" ++ show (HEIf cond e1 e2)
    c <- reduce2whnf cond
    case c of
      (HVBool cond') ->
        if cond' then
          return e1
        else
          return e2
      _                      -> trace "if" unreachable
  HELetSimple {}      -> unreachable 

interpret :: HProgram -> IO ()
interpret (Binds bs ex) = do
  let scope = Map.fromList $ map (\(Bind i e) -> (i, e)) bs
  case runRuntime (reduce2whnf ex) scope of
    Left err         -> putStrLn err
    Right v          -> print v
