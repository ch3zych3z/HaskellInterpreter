replicate' n x = if n == 0 then [] else x : replicate' (n - 1) x;

range b e = if b == e then b : [] else b : range (b + 1) e;

map' f l = case l of
{ []       -> []
; (x : xs) -> f x : map' f xs
};

main = print (map' (\x -> replicate' x x) (range 1 10))
