fib = 0 : 1 : zipWith (\a b -> a + b) fib (tail fib);

main = print (take 10 fib)