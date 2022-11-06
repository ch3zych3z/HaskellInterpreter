f y = let {
    x y = 4 + y
} in x y;

g = let {
    x = 10
} in x + f x;

main = print g
