range b e = if b == e then b : [] else b : range (b + 1) e;

main = print (map (\x -> replicate x x) (range 1 10))
