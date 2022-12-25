module TypeInferenceTest (typeInference) where

import Test.Tasty
import Test.Tasty.HUnit

import Parser
import TypeInference

typeInference :: TestTree
typeInference = testGroup "Type Inference" [exprs]

exprT :: String -> String
exprT e = 
  case parseExpr e of
    Left err   -> show err
    Right expr ->
      case infereTypeExpr expr of
        Left err -> err
        Right t  -> show t

exprTests :: [(String, String)]
exprTests = 
  [ ("1 + 2",                                                                   "Int")
  , ("\\x -> 1",                                                                "a0 -> Int")
  , ("\\f x -> f x",                                                            "(a1 -> a2) -> a1 -> a2")
  , ("\\f x -> f",                                                              "a0 -> a1 -> a0")
  , ("\\x -> 1 + 2",                                                            "a0 -> Int")
  , ("\\x -> if x then x else 1",                                               "types do not unify: Bool vs. Int")
  , ("\\x -> if x > 2 then x else 1",                                           "Int -> Int")
  , ("\\f x y -> if x == y then x + 1 else f 1",                                "(Int -> Int) -> Int -> Int -> Int")
  , ("let { f x = if x == 1 then 1 else x * f (x - 1) } in f",                  "Int -> Int")
  , ("\\f -> (\\x -> f (x x)) (\\x -> f (x x))",                                "occurs check fails: a1 ~ a1 -> a2")
  , ("let { fix f = f (fix f) } in fix",                                        "(a4 -> a4) -> a4")
  , ("let { fix f = f (fix f) } in fix (\\self x -> x * self (x - 1))",         "Int -> Int")
  , ("let { s f g x = f x (g x) } in s",                                        "(a7 -> a8 -> a9) -> (a7 -> a8) -> a7 -> a9")
  , ("\\x -> case x of { True -> False; False -> True }",                       "Bool -> Bool")
  , ("\\f x -> case f of { 0 -> f x; _ -> x }",                                 "types do not unify: a1 -> a3 vs. Int")
  , ("\\x -> case x of { 42 -> True; _ -> 1 }",                                 "types do not unify: Bool vs. Int")
  , ("\\(x:xs) -> x",                                                           "[a0] -> a0")
  , ("let { ones = 1 : ones } in ones",                                         "[Int]")
  , ("let { len xs = case xs of { [] -> 0; (_:t) -> 1 + len t }} in len",       "[a7] -> Int")
  , ("\\(x, y) -> x + y",                                                       "(Int,Int) -> Int")
  , ("\\(x, y) -> (y, [x, x])",                                                 "(a0,a1) -> (a1,[a0])")
  , ("\\x y -> case (x, y) of { (Just 1, Nothing) -> x; _ -> y }",              "(Maybe Int) -> (Maybe Int) -> Maybe Int")
  , ("let { head l = case l of { [] -> Nothing; (x:_) -> Just x} } in head",    "[a6] -> Maybe a6")
  ]

exprs :: TestTree
exprs = testGroup "Expressions" $ map (\(n, ans) -> testCase n $ exprT n @?= ans) exprTests
