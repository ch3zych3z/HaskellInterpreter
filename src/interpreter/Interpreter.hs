{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TupleSections #-}
module Interpreter (interpret) where

import qualified Data.Map as Map
import Prelude hiding (exp)
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace
import Control.Monad (forM, zipWithM)

import Pattern
import Ast
import Runtime
import qualified Exception

unreachable :: a
unreachable = error "error: unreachable code fragment"

incorrectOperationType :: HBinOp -> String -> a
incorrectOperationType op t = error $ "error: " ++ show op ++ " is not type of " ++ t

-- Pattern matching

instance Matchable HValuePat where
  match (HVPInt v1)  (HEVal (HVInt v2))  | v1 == v2 = return $ Just Map.empty
  match (HVPBool v1) (HEVal (HVBool v2)) | v1 == v2 = return $ Just Map.empty
  match _            _                   = return Nothing

instance Matchable HPattern where
  match HPWildcard               _ = return $ Just Map.empty
  match (HPIdent i)              e = return $ Just $ Map.singleton i e
  match (HPLabel is p)           e = do
    matched <- match p e
    case matched of
      Nothing -> return Nothing
      Just sc -> do
        return $ Just $ foldr (uncurry Map.insert . (, e)) sc is
  match (HPVal v)                e = reduce2whnf e >>= match v
  match (HPList HLPNil)          e = do
    e' <- reduce2whnf e
    case e' of
      HETypeCons (HCList HLNil) -> return $ Just Map.empty
      _                         -> return Nothing
  match (HPList (HLPCons hp tp)) e = do
    e' <- reduce2whnf e
    case e' of
      HETypeCons (HCList (HLCons h t)) -> do
        matchedHead <- match hp h
        matchedTail <- match tp t
        case (matchedHead, matchedTail) of
          (Just sc1, Just sc2) -> return $ Just $ sc1 `Map.union` sc2
          _                    -> return Nothing
      _                                -> return Nothing
  match (HPTuple s ps)           e = do
    e' <- reduce2whnf e
    case e' of
      HETypeCons (HCTuple s' es) ->
        if s' /= s then return Nothing
        else do
          scs <- zipWithM match ps es
          case sequence scs of
            Just scs' -> return $ Just $ foldl1 Map.union scs'
            Nothing   -> return Nothing
      _                          -> return Nothing
  match (HPMaybe Nothing)        e = do
    e' <- reduce2whnf e
    case e' of
      HETypeCons (HCMaybe Nothing) -> return $ Just Map.empty
      _                            -> return Nothing
  match (HPMaybe (Just p))       e = do
    e' <- reduce2whnf e
    case e' of
      HETypeCons (HCMaybe (Just expr)) -> match p expr
      _                                -> return Nothing
          

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
subst (HECase to ms)          i e                       = 
  let
    e' = subst to i e
    substMatch m@(p :->: expr) | i `Pattern.elem` p = m
                               | otherwise  = p :->: subst expr i e
    ms' = map substMatch ms
  in HECase e' ms'
subst (HETypeCons (HCList l)) i e                       =
  case l of
    HLNil      -> nilExpr
    HLCons h t -> consExpr (subst h i e) (subst t i e)
subst (HETypeCons (HCTuple s es)) i e                   = HETypeCons $ HCTuple s $ map (\ex -> subst ex i e) es
subst (HETypeCons (HCMaybe m))    i e                   = case m of
  Just expr -> HETypeCons $ HCMaybe $ Just $ subst expr i e
  Nothing   -> HETypeCons $ HCMaybe Nothing

setScopes :: Scope -> HExpr -> HExpr
setScopes scope v@(HEVar sc n) | Map.member n scope &&
                                 Map.null sc           = HEVar scope n
                               | otherwise             = v
setScopes _     v@(HEVal _)                            = v
setScopes sc      (HEApp e1 e2)                        = HEApp (setScopes sc e1) (setScopes sc e2)
setScopes sc      (HEAbs n e)                          = HEAbs n $ setScopes sc e
setScopes sc      (HELet (Binds bs e))                 =
  let setScopesBinds = map $ \(Bind i exp) -> Bind i $ setScopes sc' exp
      sc'            = sc Map.\\ binds2Scope bs
  in HELet $ Binds (setScopesBinds bs) $ setScopes sc' e
setScopes sc      (HEBinOp e1 op e2)                   = HEBinOp (setScopes sc e1) op (setScopes sc e2)
setScopes sc      (HEIf e1 e2 e3)                      = HEIf (setScopes sc e1) (setScopes sc e2) (setScopes sc e3)
setScopes sc      (HECase e ms)                        =
  let
    e' = setScopes sc e
    ids p = Pattern.collectIdents [p]
    scope = foldr Map.delete sc . ids
    setScopesMatching (p :->: expr) = p :->: setScopes (scope p) expr
    ms' = map setScopesMatching ms
  in HECase e' ms'
setScopes sc      (HETypeCons (HCTuple s es))          = HETypeCons $ HCTuple s $ map (setScopes sc) es
setScopes _       (HETypeCons (HCList HLNil))          = nilExpr
setScopes sc      (HETypeCons (HCList (HLCons e1 e2))) = consExpr (setScopes sc e1) (setScopes sc e2)
setScopes _     m@(HETypeCons (HCMaybe Nothing))       = m
setScopes sc      (HETypeCons (HCMaybe (Just e)))      = HETypeCons $ HCMaybe $ Just $ setScopes sc e

apply :: HExpr -> HExpr -> Runtime HExpr
apply (HEAbs p to) e = do
  matched <- match p e
  case matched of
    Just sc -> return $ Map.foldlWithKey subst to sc
    Nothing -> Exception.incompletePM
apply _ _            = trace "apply" unreachable

toArithm :: HBinOp -> Int -> Int -> Int
toArithm = \case
  Add -> (+)
  Mul -> (*)
  Sub -> (-)
  Div -> div
  Rem -> mod
  op  -> incorrectOperationType op "Arithmetics"

toOrd :: HBinOp -> Int -> Int -> Bool
toOrd = \case
  Gr   -> (>)
  Le   -> (<)
  Greq -> (>=)
  Leq  -> (<=)
  op   -> incorrectOperationType op "Orders"

-- Interpreter

isWHNF :: HExpr -> Bool
isWHNF (HEVal _)          = True
isWHNF (HEAbs _ _)        = True
isWHNF (HETypeCons _)     = True
isWHNF _                  = False

reduce2whnf :: HExpr -> Runtime HExpr
reduce2whnf e | isWHNF e  = return e
              | otherwise = reduce e >>= reduce2whnf

eqls :: HExpr -> HExpr -> Runtime Bool
eqls (HETypeCons (HCList (HLCons h1 t1))) (HETypeCons (HCList (HLCons h2 t2))) = do
  h1' <- reduce2whnf h1
  h2' <- reduce2whnf h2
  v1 <- eqls h1' h2'
  if v1 then do
    t1' <- reduce2whnf t1
    t2' <- reduce2whnf t2
    v2 <- eqls t1' t2'
    return $ v1 && v2
  else return False
eqls (HETypeCons (HCList HLNil)) (HETypeCons (HCList HLNil))                   = return True
eqls (HEVal v1) (HEVal v2)                                                     = return $ v1 == v2
eqls (HETypeCons (HCMaybe Nothing)) (HETypeCons (HCMaybe Nothing))             = return True
eqls (HETypeCons (HCMaybe (Just e1))) (HETypeCons (HCMaybe (Just e2)))         = do
  e1' <- reduce2whnf e1
  e2' <- reduce2whnf e2
  eqls e1' e2'
eqls (HETypeCons (HCTuple s1 e1s)) (HETypeCons (HCTuple s2 e2s)) | s1 == s2    = and <$> zipWithM eqls e1s e2s
eqls _ _                                                                       = return False

reduceOp :: HBinOp -> HExpr -> HExpr -> Runtime HExpr
reduceOp op v1 v2
  | op `List.elem` [Add, Mul, Sub, Div, Rem] = do
    let (HEVal (HVInt a)) = v1
        (HEVal (HVInt b)) = v2
    if b == 0 && (op `List.elem` [Div, Rem]) then
      Exception.throwZeroDivision
    else
      return $ HEVal $ HVInt $ toArithm op a b
  | op == Eqls                               = HEVal . HVBool <$> eqls v1 v2
  | op `List.elem` [Gr, Le, Greq, Leq]       = do
    let (HEVal (HVInt a)) = v1
        (HEVal (HVInt b)) = v2
    return $ HEVal $ HVBool $ toOrd op a b
reduceOp _ _ _                               = trace "reduce op" unreachable

reduce :: HExpr -> Runtime HExpr
reduce = \case
  HEApp e1 e2         -> do
    e1' <- reduce2whnf e1
    apply e1' e2
  val@(HEVal _)       -> traceM ("reducing val:\n" ++ show val) >> return val
  v@(HEVar sc i)      -> do
    pushScope sc
    e <- setScopes sc . fromMaybe v <$> getVar i
    popScope
    return e
  a@(HEAbs _ _)       -> traceM ("reducing abstraction:\n" ++ show a) >> return a
  HELet (Binds bs e)  -> do
    let sc = binds2Scope bs
    return $ setScopes sc e
  HEBinOp e1 op e2    -> do
    e1' <- reduce2whnf e1
    e2' <- reduce2whnf e2
    reduceOp op e1' e2'
  HEIf cond e1 e2     -> do
    c <- reduce2whnf cond
    case c of
      (HEVal(HVBool cond')) ->
        if cond' then
          return e1
        else
          return e2
      _                      -> trace "if" unreachable
  HECase e ms         -> do
    res <- forM ms $ \(p :->: expr) -> do
      scope <- match p e
      case scope of
        Just sc -> return $ Just (sc, expr)
        Nothing -> return Nothing
    case List.find isJust res of
      Just (Just (scope, expr)) -> return $ setScopes scope expr
      _                      -> Exception.incompletePM
  tc@HETypeCons {}       -> return tc

reduce2show :: HExpr -> Runtime HShow
reduce2show (HEVal x)                          = case x of
  HVInt  i -> return $ HSInt  i
  HVBool b -> return $ HSBool b
reduce2show (HETypeCons (HCList HLNil))        = return $ HSList []
reduce2show (HETypeCons (HCList (HLCons h t))) = do
  hsh <- reduce2show h
  tsh <- reduce2show t
  let (HSList tsh') = tsh
  return $ HSList $ hsh : tsh'
reduce2show (HETypeCons (HCTuple _ es))        = HSTuple <$> mapM reduce2show es
reduce2show (HETypeCons (HCMaybe m))           = do
  case m of
    Nothing -> return $ HSMaybe Nothing
    Just e  -> HSMaybe . Just <$> reduce2show e
reduce2show e                                  = reduce2whnf e >>= reduce2show

interpret :: HProgram -> IO ()
interpret (Binds bs ex) = do
  let scope = Map.fromList $ map (\(Bind i e) -> (i, e)) bs
  case runRuntime (reduce2show ex) scope of
    Left err         -> putStrLn err
    Right v          -> print v
