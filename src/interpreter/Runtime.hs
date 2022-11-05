module Runtime where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Ast

-- type Scope = Map.Map HId HExpr

newtype HRuntime = HRuntime {
   scopes :: [Scope]
}

type Runtime a = ExceptT String (State HRuntime) a

runRuntime :: Runtime a -> Scope -> Either String a
runRuntime r gs = evalState (runExceptT r) initHRuntime
  where initHRuntime = HRuntime { scopes = [gs] }

getScopes :: Runtime [Scope]
getScopes = scopes <$> get

modifyScopes :: ([Scope] -> [Scope]) -> Runtime ()
modifyScopes f = do
  newScopes <- f <$> getScopes
  modify $ \rt -> rt { scopes = newScopes }

pushScope :: Scope -> Runtime ()
pushScope s = modifyScopes (s :)

popScope :: Runtime ()
popScope = modifyScopes tail

binds2Scope :: [Binding] -> Scope
binds2Scope = Map.fromList . map (\(Bind i e) -> (i, e))

pushBindings :: [Binding] -> Runtime ()
pushBindings = pushScope . binds2Scope

getVar :: HId -> Runtime (Maybe HExpr)
getVar i = 
  let lookupScopes _ []         = Nothing
      lookupScopes i (sc : scs) =
        case Map.lookup i sc of
          e@(Just _)  -> e
          Nothing     -> lookupScopes i scs
  in lookupScopes i <$> getScopes

-- lookupScope :: HId -> HRuntime (Maybe HExpr)
-- lookupScope i = Map.lookup i <$> getScope