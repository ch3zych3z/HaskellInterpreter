foo x = case x of { (True, v) -> 1; (False, u) -> 0 };

main = print (foo (True, 1 `div` 0))