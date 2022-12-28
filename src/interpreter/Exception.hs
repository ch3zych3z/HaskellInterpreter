module Exception (module Exception) where

import Control.Monad.Except

import Runtime

throwException :: String -> Runtime a
throwException s = throwError $ "exception: " ++ s

throwZeroDivision :: Runtime a
throwZeroDivision = throwException "division by zero!"

incompletePM :: Runtime a
incompletePM = throwException "incomplete pattern matching"
