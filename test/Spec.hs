import Test.Tasty

import TypeInferenceTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Integration" [typeInference]
